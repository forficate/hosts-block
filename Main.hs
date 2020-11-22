{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           GHC.Show                                 ( showLitString )

import           Data.Set                                 ( Set )
import qualified Data.Set                      as Set

import           Options.Applicative
import           Data.Semigroup                           ( (<>) )

import           Data.List                                ( partition
                                                          , foldl'
                                                          )
import           Data.Maybe                               ( maybeToList )

import           Control.Applicative                      ( (<|>) )
import           Toml                                     ( TomlBiMap
                                                          , TomlCodec
                                                          , (.=)
                                                          )
import qualified Toml

import           Data.Text                                ( Text )
import qualified Data.Text.IO                  as TIO

import           Data.Map                                 ( Map )
import qualified Data.Map.Strict               as Map

import           Control.Concurrent                hiding ( yield )
import           Control.Monad
import           Control.Monad.Trans                      ( liftIO
                                                          , MonadIO(..)
                                                          )


import           Network.HTTP.Conduit
import           Conduit                                  ( runConduit
                                                          , (.|)
                                                          )
import           Control.Monad.Trans.Resource             ( MonadResource(..)
                                                          , runResourceT
                                                          )

import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T

import           Data.ByteString                          ( ByteString(..) )
import           Data.Conduit                             ( ConduitT
                                                          , ConduitM
                                                          )
import qualified Data.Conduit                  as C
import qualified Data.Conduit.Text             as C
import qualified Data.Conduit.Combinators      as C

import           Data.Bifunctor                           ( first )

import           System.IO                                ( stderr
                                                          , stdout
                                                          , hSetBuffering
                                                          , BufferMode(..)
                                                          )

import           HostParser

import           Data.Char                                ( isAscii )


import           System.Exit

import qualified Data.ByteString.Char8         as BS

import           UnliftIO.Async                           ( pooledMapConcurrentlyN
                                                          )


data CliOptions = CliOptions
    { cliConfigSrc :: String
    , cliTargetIp :: Maybe String
    , cliOutputFormat :: Maybe OutputFormat
    }

data ConfigFile = ConfigFile
  { configFileSource :: Map Text Source } deriving (Show)


data Source = Source
    { unSourceUrl :: !Text } deriving (Eq, Show)

data Task = Process (Text, Source) | Done deriving (Eq, Show)


data Action = LogParseError Text | Ignore | WriteLine Text deriving (Eq, Show)

data OutputFormat = HostsFormat | UnboundFormat deriving (Eq, Show)

-- ==================================
-- CLI arg parsing stuff
-- ==================================
cliOptions :: Parser CliOptions
cliOptions =
  CliOptions
    <$> strOption (long "conf" <> metavar "FILE" <> help "Config file location")
    <*> optional
          (  strOption
          $  long "ip"
          <> metavar "IP"
          <> help
               "Target IP, defaults to 0.0.0.0. If set this supersedes the value in the conf file if set"
          )
    <*> optional
          (  option outputFormat
          $  long "format"
          <> short 'f'
          <> help
               "Output format. Valid values hosts|unbound. Defaults to hosts if not set"
          )
 where
  outputFormat = maybeReader $ \case
    "hosts"   -> Just HostsFormat
    "unbound" -> Just UnboundFormat
    _         -> Nothing


opts :: ParserInfo CliOptions
opts = info
  (cliOptions <**> helper)
  (  fullDesc
  <> progDesc
       "Generate a lists of DNS hosts to block from configurable source databases"
  <> header "Host blocklist generator"
  )



-- ==================================
-- Config parsing file stuff
-- ==================================
configFileCodec :: Toml.TomlCodec ConfigFile
configFileCodec =
  ConfigFile
    <$> Toml.tableMap Toml._KeyText (Toml.table sourceCodec) "source"
    .=  configFileSource

sourceCodec :: TomlCodec Source
sourceCodec = Source <$> Toml.text "url" .= unSourceUrl


-- ==================================
-- Main app stuff
-- ==================================
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  cliOpts    <- execParser opts
  configToml <- TIO.readFile (cliConfigSrc cliOpts)
  let res = Toml.decode configFileCodec configToml
  case res of
    Left  err      -> die $ show err
    Right settings -> do
      let targetIp' = maybe "0.0.0.0" T.pack (cliTargetIp cliOpts)

      -- Split the sources in to buckets, remote http sources and local hosts files on disc
      let sources = Map.toAscList (configFileSource settings)

      httpManager <- newManager tlsManagerSettings

      allHosts    <- pooledMapConcurrentlyN 4 (getRemoteHosts httpManager) sources

      -- Can't use set in pooledMapConcurrentlyn as no Traversable instance.
      -- So we convert the List of results to a Set for de-duplication after we aggregated all results
      -- Also make sure localhost is never included in the blocklist
      let deDupedHosts = Set.delete "localhost" $ foldl' (\b a -> Set.union b $ Set.fromList a) Set.empty allHosts


      forM_ deDupedHosts $ \s -> case cliOutputFormat cliOpts of
        Just UnboundFormat ->
          TIO.putStrLn ("local-zone: \"" <> s <> "\" redirect") *>
            TIO.putStrLn ("local-data: \"" <> s <> " A " <> targetIp' <> "\"")
        _ ->
          TIO.putStrLn $ targetIp' <> " " <> s




getRemoteHosts :: MonadIO m => Manager -> (Text, Source) -> m [Text]
getRemoteHosts httpManager (key, source) = do
  request <- liftIO $ parseRequest (T.unpack $ unSourceUrl source)
  liftIO $ runResourceT $ do
    response <- http request httpManager
    runConduit
      $  responseBody response
      .| C.linesUnboundedAscii
      .| C.decodeUtf8
      .| discardNonAscii
      .| hostLines
      .| asAction
      .| actionSink key


-- Conduit taking a Text as a value outputting a HostFileLine parse result
hostLines :: Monad m => ConduitT Text (Either Text HostFileLine) m ()
hostLines = C.map $ \s -> first (const s) (hostFileLine s)

-- Conduit taking an `Either Text HostFileLine` outputting an action to perform
asAction :: Monad m => ConduitT (Either Text HostFileLine) Action m ()
asAction = C.map $ \case
  Left  s             -> LogParseError s
  Right (Host _ name) -> WriteLine name
  Right _             -> Ignore

actionSink :: (MonadIO m, MonadResource m) => Text -> ConduitT Action C.Void m [Text]
actionSink s = C.foldMapM $ \case
  LogParseError e -> do
    liftIO $ TIO.hPutStrLn stderr ("[WARN] "<> s <> " skipping unknown line \"" <> T.pack (showLitString (T.unpack e) "") <> "\"")
    return []
  WriteLine t ->
    return [T.toLower t]
  Ignore  ->
    return []


-- Filter out any bad host lines ie the following was appearing in a host file: secret.ɢoogle.com
discardNonAscii :: Monad m => ConduitT Text Text m ()
discardNonAscii =
  C.concatMap $ \s -> maybe [s] (const []) (T.find (not . isAscii) s)
