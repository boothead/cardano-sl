module VerificationBench where

import           Universum

import           Control.Monad.Random.Strict (evalRandT)
import           Control.DeepSeq (force)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import           Data.Time.Units (Microsecond, convertUnit)
import           Formatting (sformat, shown, stext, (%))
import qualified GHC.Exts as IL
import qualified Options.Applicative as Opts
import           System.Directory (doesFileExist)
import           System.Random (newStdGen)
import           System.Wlog (LoggerName (..), LoggerConfig, consoleActionB, debugPlus, setupLogging, defaultHandleAction, termSeveritiesOutB, consoleActionB, logInfo, logDebug, logError)

import           Mockable.CurrentTime (realTime)

import           Pos.AllSecrets (mkAllSecretsSimple)
import           Pos.Binary.Class (serialize, decodeFull)
import           Pos.Block.Error (ApplyBlocksException, VerifyBlocksException)
import           Pos.Block.Logic.VAR (getVerifyBlocksContext', verifyAndApplyBlocks, verifyBlocksPrefix, rollbackBlocks)
import           Pos.Core (Block, headerHash)
import           Pos.Core.Common (BlockCount (..), unsafeCoinPortionFromDouble)
import           Pos.Core.Configuration (genesisBlockVersionData, genesisData, genesisSecretKeys)
import           Pos.Core.Genesis (FakeAvvmOptions (..), GenesisData (..), GenesisInitializer (..), TestnetBalanceOptions (..))
import           Pos.Core.Slotting (Timestamp (..))
import           Pos.DB.DB (initNodeDBs)
import           Pos.Generator.Block (BlockGenParams (..), TxGenParams (..), genBlocksNoApply)
import           Pos.Launcher.Configuration (ConfigurationOptions (..), HasConfigurations, defaultConfigurationOptions, withConfigurationsM)
import           Pos.Txp.Logic.Global (txpGlobalSettings)
import           Pos.Util.Chrono (OldestFirst (..), NE, nonEmptyNewestFirst)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo, retrieveCompileTimeInfo)
import           Test.Pos.Block.Logic.Mode (BlockTestMode, TestParams (..), runBlockTestMode)

genesisInitializer :: GenesisInitializer
genesisInitializer = GenesisInitializer
    { giTestBalance = balance
    , giFakeAvvmBalance = FakeAvvmOptions
          { faoCount = 1
          , faoOneBalance = maxBound
          }
    , giAvvmBalanceFactor = unsafeCoinPortionFromDouble 0
    , giUseHeavyDlg = False
    , giSeed = 0
    }

balance :: TestnetBalanceOptions
balance = TestnetBalanceOptions
    { tboPoors = 1
    , tboRichmen = 1
    , tboTotalBalance = maxBound
    , tboRichmenShare = 1
    , tboUseHDAddresses = False
    }

generateBlocks :: HasConfigurations => BlockCount -> BlockTestMode (OldestFirst NE Block)
generateBlocks bCount = do
    g <- liftIO $ newStdGen
    let secretKeys =
            case genesisSecretKeys of
                Nothing ->
                    error "generateBlocks: no genesisSecretKeys"
                Just ks -> ks
    bs <- flip evalRandT g $ genBlocksNoApply
            (BlockGenParams
                { _bgpSecrets = mkAllSecretsSimple secretKeys
                , _bgpBlockCount = bCount
                , _bgpTxGenParams = TxGenParams
                    { _tgpTxCountRange = (0, 2)
                    , _tgpMaxOutputs = 2
                    }
                , _bgpInplaceDB = False
                , _bgpSkipNoKey = True
                , _bgpGenStakeholders = gdBootStakeholders genesisData
                , _bgpTxpGlobalSettings = txpGlobalSettings
                })
            maybeToList
    return $ OldestFirst $ NE.fromList bs


data BenchArgs = BenchArgs
    { baConfigPath :: FilePath
    , baConfigKey  :: Text
    , baBlockCount :: BlockCount
    , baRuns       :: Int
    , baApply      :: Bool
    , baBlockCache :: Maybe FilePath
    }

configPathP :: Opts.Parser FilePath
configPathP = Opts.strOption $
       Opts.long "config"
    <> Opts.value "lib/configuration.yaml"
    <> Opts.showDefault
    <> Opts.help "path to yaml configuration file"

configKeyP :: Opts.Parser String
configKeyP = Opts.strOption $
       Opts.long "config-key"
    <> Opts.value "bench-validation"
    <> Opts.showDefault
    <> Opts.help "configuration key"

blockCountP :: Opts.Parser BlockCount
blockCountP = Opts.option (BlockCount <$> Opts.auto) $
       Opts.long "block-count"
    <> Opts.value 2000
    <> Opts.showDefault
    <> Opts.help "number of blocks to generate"

runsP :: Opts.Parser Int
runsP = Opts.option Opts.auto $
       Opts.long "runs"
    <> Opts.short 'r'
    <> Opts.value 100
    <> Opts.showDefault
    <> Opts.help "number of runs over generated blocks"

applyBlocksP :: Opts.Parser Bool
applyBlocksP = Opts.switch $
       Opts.long "apply"
    <> Opts.short 'a'
    <> Opts.help "apply blocks: runs `verifyAndApplyBlocks` otherwise it runs `verifyBlocksPrefix`"

blockCacheP :: Opts.Parser (Maybe FilePath)
blockCacheP = Opts.optional $ Opts.strOption $
       Opts.long "block-cache"
    <> Opts.help "path to block cache (file where generated blocks are written / read from)"

benchArgsParser :: Opts.Parser BenchArgs
benchArgsParser = BenchArgs
    <$> configPathP
    <*> (T.pack <$> configKeyP)
    <*> blockCountP
    <*> runsP
    <*> applyBlocksP
    <*> blockCacheP

-- | Write generated blocks to a file.
writeBlocks :: FilePath -> OldestFirst NE Block -> IO ()
writeBlocks path bs = do
    let sbs = serialize bs
    BSL.writeFile path sbs

-- | Read generated blocks from a file.
readBlocks :: FilePath -> IO (Maybe (OldestFirst NE Block))
readBlocks path = do
    sbs <- BSL.readFile path
    case decodeFull sbs of
        Left err -> do
            putStrLn err
            return Nothing
        Right bs -> return (Just bs)

main :: IO ()
main = do
    setupLogging Nothing loggerConfig
    args <- Opts.execParser
        $ Opts.info
            (benchArgsParser <**> Opts.helper)
            (Opts.fullDesc <> Opts.progDesc
                (  "The program generates given ammount of blocks and applies them. "
                )
            )
    startTime <- realTime
    let co = defaultConfigurationOptions
            { cfoFilePath = baConfigPath args
            , cfoKey = baConfigKey args
            , cfoSystemStart = Just (Timestamp startTime)
            }
    withCompileInfo $(retrieveCompileTimeInfo) $
        withConfigurationsM (LoggerName "verification-bench") co $ \_ ->
            let tp = TestParams
                    { _tpStartTime = Timestamp (convertUnit startTime)
                    , _tpBlockVersionData = genesisBlockVersionData
                    , _tpGenesisInitializer = genesisInitializer
                    }
            in runBlockTestMode tp $ do
                -- initialize databasea
                initNodeDBs
                bs <- case baBlockCache args of
                    Nothing -> do
                        -- generate blocks and evaluate them to normal form
                        logInfo "Generating blocks"
                        generateBlocks (baBlockCount args)
                    Just path -> do
                        fileExists <- liftIO $ doesFileExist path
                        if fileExists
                            then do
                                liftIO (readBlocks path) >>= \case
                                    Nothing -> do
                                        -- generate blocks and evaluate them to normal form
                                        logInfo "Generating blocks"
                                        bs <- generateBlocks (baBlockCount args)
                                        liftIO $ writeBlocks path bs
                                        return bs
                                    Just bs -> return bs
                            else do
                                -- generate blocks and evaluate them to normal form
                                logInfo "Generating blocks"
                                bs <- generateBlocks (baBlockCount args)
                                liftIO $ writeBlocks path bs
                                return bs

                logDebug $ sformat ("generated blocks:\n\t"%stext) $ T.intercalate "\n\t" $ map (show . headerHash) (IL.toList bs)
                let bss = force $ replicate (baRuns args) bs

                logInfo "Verifying blocks"
                (times, errs) <- unzip <$> forM bss
                    (if baApply args
                        then validateAndApply
                        else validate)

                let -- drop first three results (if there are more than three results)
                    itimes :: [Float]
                    itimes = map realToFrac (if length times > 3 then drop 3 times else times)
                    -- execution mean time
                    mean :: Float
                    mean = avarage itimes
                    -- standard deviation of the execution time distribution
                    stddev :: Float
                    stddev = sqrt . (\x -> x / realToFrac (length itimes - 1)) . avarage . map ((**2) . (-) mean) $ itimes
                logInfo $ sformat ("verification and application mean time: "%shown%"μs stddev: "%shown) mean stddev

                -- print errors
                let errs' = catMaybes errs
                    errno = length errs'
                when (errno > 0) $ do
                    logError $ sformat ("Verification/Application errors ("%shown%"):") errno
                    traverse_ (logError . show) errs
    where
        loggerConfig :: LoggerConfig
        loggerConfig = termSeveritiesOutB debugPlus
                <> consoleActionB defaultHandleAction

        avarage :: [Float] -> Float
        avarage as = sum as / realToFrac (length as)

        validate
            :: ( HasConfigurations
               , HasCompileInfo
               )
            => OldestFirst NE Block
            -> BlockTestMode (Microsecond, Maybe (Either VerifyBlocksException ApplyBlocksException))
        validate blocks = do
            verStart <- realTime
            -- omitting current slot for simplicity
            ctx <- getVerifyBlocksContext' Nothing
            res <- (force . either Left (Right . fst)) <$> verifyBlocksPrefix ctx blocks
            verEnd <- realTime
            return (verEnd - verStart, either (Just . Left) (const Nothing) res)

        validateAndApply
            :: ( HasConfigurations
               , HasCompileInfo
               )
            => OldestFirst NE Block
            -> BlockTestMode (Microsecond, Maybe (Either VerifyBlocksException ApplyBlocksException))
        validateAndApply blocks = do
            verStart <- realTime
            ctx <- getVerifyBlocksContext' Nothing
            res <- force <$> verifyAndApplyBlocks ctx False blocks
            verEnd <- realTime
            case res of
                Left _ -> return ()
                Right (_, blunds)
                    -> whenJust (nonEmptyNewestFirst blunds) rollbackBlocks
            return (verEnd - verStart, either (Just . Right) (const Nothing) res)
