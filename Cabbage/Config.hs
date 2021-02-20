{-# Language RecordWildCards #-}
{-# Language OverloadedStrings #-}

module Cabbage.Config where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable
import           System.Directory
import           System.Environment.XDG.BaseDir (getUserConfigDir)
import           System.IO.Error
import           Text.Read

import           Cabbage.Cabal as D

throwString :: String -> IO a
throwString err = ioError (userError err)

data Config = Config {
  installed :: Map D.PackageName D.Version,
  implicit :: Map D.PackageName D.Version,
  constraints :: Map D.PackageName D.VersionRange,
  localPackages :: Set Text,
  confProject :: Text
  }
  deriving (Eq, Show, Read)

defaultConfig :: Config
defaultConfig = Config Map.empty Map.empty Map.empty Set.empty ""

readConfig :: IO Config
readConfig = do
  configDir <- getUserConfigDir "cabbage"
  txt <- T.readFile (configDir ++ "/cabbage.conf")
  proj <- T.readFile (configDir ++ "/cabal.project") <|> pure ""
  case parseNewConfig txt of
    Right config -> return config { confProject = proj }
    Left err -> ioError (userError err)

writeConfig :: Config -> IO ()
writeConfig config = writeConfigText (encodeConfig config)

encodeConfig :: Config -> Text
encodeConfig Config{..} = T.unlines parts
  where
    parts = fold [
      [T.unwords ["installed", pkg, ver] | (pkg, ver) <- Map.toList installed],
      [T.unwords ["implicit", pkg, ver] | (pkg, ver) <- Map.toList implicit],
      [T.unwords ["constraint", pkg, ver] | (pkg, ver) <- Map.toList constraints],
      [T.unwords ["local", path] | path <- Set.toList localPackages]
      ]

parseNewConfig :: Text -> Either String Config
parseNewConfig text = do
  effects <- for (T.lines text) $ \line -> do
    case T.words line of
      ["installed", sPkg, sVer] -> addInstalled (sPkg) <$> Right sVer
      ["implicit", sPkg, sVer] -> addImplicit (sPkg) <$> Right sVer
      ("constraint":sPkg:sVer) -> addConstraint (sPkg) <$> Right (T.unwords sVer)
      ["local", path] -> pure (addLocalPackage path)
      _ -> Left (T.unpack ("Couldn't parse line: " <> line))
  return (foldr (.) id effects defaultConfig)
    where
      addInstalled pkg ver Config{..} = Config{installed = Map.insert pkg ver installed, ..}
      addImplicit pkg ver Config{..} = Config{implicit = Map.insert pkg ver implicit, ..}
      addConstraint pkg vers Config{..} = Config{constraints = Map.insert pkg vers constraints, ..}
      addLocalPackage path Config{..} = Config{localPackages = Set.insert path localPackages, ..}

writeConfigText :: Text -> IO ()
writeConfigText config = do
  configDir <- getUserConfigDir "cabbage"
  createDirectoryIfMissing True configDir
  let targetFile = configDir ++ "/cabbage.conf"
  doesFileExist targetFile >>= (flip when $ do
    renameFile targetFile (targetFile ++ "~"))
  T.writeFile targetFile config
