module Main where

import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Traversable
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.Environment.XDG.BaseDir
import           System.Exit
import           System.FilePath.Glob (glob)
import           System.FilePath.Posix ((</>), takeBaseName)
import           System.IO
import           System.IO.Temp (withSystemTempDirectory, getCanonicalTemporaryDirectory, createTempDirectory)
import           System.Process (callProcess)
import           Text.Pretty.Simple
import           Text.Printf

import qualified Cabbage.Cabal as D
import           Cabbage.Config (Config(..), readConfig, writeConfig, encodeConfig)
import qualified Cabbage.Config as Config
import           Cabbage.Parser

localName :: FilePath -> IO Text
localName path = do files <- map (T.splitOn "." . T.pack) <$> listDirectory path
                    return (head [base | [base, ext] <- files, ext == "cabal"])


-- parsePackages :: [String] -> Map D.PackageName (Maybe D.Version)
-- parsePackages deps = Map.fromList [(T.pack dep, Nothing) | dep <- deps]

-- parseConstraints :: [String] -> Map D.PackageName D.VersionRange
-- parseConstraints deps = Map.fromList [(D.depPackage dep, D.depVersion dep) | dep <- map (D.parseDep . T.pack) deps]

writeDefaultEnvironmentFile :: Text -> String -> IO ()
writeDefaultEnvironmentFile txt ghcver = do
  home <- getHomeDirectory
  let targetDir = home ++ "/.ghc/" ++ ghcver ++ "/environments/"
      targetFile = home ++ "/.ghc/" ++ ghcver ++ "/environments/default"
  createDirectoryIfMissing True targetDir

  doesFileExist targetFile >>= (flip when $ do
    renameFile targetFile (targetFile ++ "~"))
  T.writeFile targetFile txt
  putStrLn ("Wrote default environment file " <> targetFile)

data PS = PS {
  pVersion :: Maybe D.Version,
  pVersionRange :: Maybe D.VersionRange,
  pExplicit :: Bool
  }
  deriving Show

data Plan = Plan {
  pInstall :: Map D.PackageName PS,
  pLocals :: Set Text,
  pProject :: Text,
  pOutputFile :: Maybe Text
  }
  deriving Show

alterPackage :: D.PackageName -> (Maybe PS -> Maybe PS) -> Plan -> Plan
alterPackage pkg f plan = plan { pInstall = Map.alter f pkg (pInstall plan) }

confPlan :: Config -> Plan
confPlan conf = Plan pi pl (confProject conf) Nothing
  where
    build e v = PS { pVersion = Just v,
                     pVersionRange = Nothing,
                     pExplicit = e}
    pi = (build True <$> installed conf) <> (build False <$> implicit conf)
    pl = localPackages conf

planConf :: Plan -> Config
planConf plan = Config { installed = fromJust . pVersion <$> Map.filter pExplicit (pInstall plan),
                         implicit = fromJust . pVersion <$> Map.filter (not . pExplicit) (pInstall plan),
                         constraints = Map.empty,
                         localPackages = pLocals plan,
                         confProject = pProject plan
                         }

-- General merging function for Maps.
plusMap :: Ord k => (k -> Maybe a -> Maybe b -> Maybe c) -> Map k a -> Map k b -> Map k c
plusMap f ma mb = Map.mapMaybe id $ Map.fromSet go (Map.keysSet ma <> Map.keysSet mb)
  where
    go k = f k (Map.lookup k ma) (Map.lookup k mb)

implicitConstraints :: Plan -> [Text]
implicitConstraints plan = [T.pack $ printf "any.%s %s" pkg (D.thisVersion v)
                           | (pkg, PS{pExplicit=False, pVersion = Just v}) <- Map.toList (pInstall plan)]

prepareBuildDir :: Plan -> IO FilePath
prepareBuildDir plan = do
  let
    dep pkg PS{pExplicit = True, pVersion = Just v} = [D.Dependency pkg (D.thisVersion v)]
    dep pkg PS{pExplicit = True, pVersionRange = Just vr} = [D.Dependency pkg vr]
    dep pkg ps = []
    deps = fold [dep pkg ps | (pkg, ps) <- Map.toList (pInstall plan)]
    locals = T.unpack <$> Set.toList (pLocals plan)
    implicits = implicitConstraints plan
  root <- getUserCacheDir "cabbage"
  tmpdir <- createTempDirectory root "build"
  localnames <- for locals $ \lpath -> do
    name <- localName lpath
    createDirectoryLink lpath (tmpdir </> T.unpack name)
    return name
  let pkgText = D.makeDummyPackage (deps ++ [D.anyVersionDep n | n <- localnames, not (elem n [d | D.Dependency d _ <- deps])])
  T.writeFile (tmpdir </> "dummy.cabal") pkgText
  let projectText = T.unlines (["packages: dummy.cabal" <> if not (null localnames) then " */*.cabal" else ""] ++
                               ["constraints: " <> T.intercalate ", " implicits | not (null implicits)] ++
                               [pProject plan])
  T.writeFile (tmpdir </> "cabal.project") projectText
  putStrLn ("Created temp dir: " ++ tmpdir)
  return tmpdir

getVersions :: Plan -> IO [(D.PackageName, D.Version)]
getVersions plan = do
  path <- prepareBuildDir plan
  withCurrentDirectory path $ do
    callProcess "cabal" ["v2-freeze"]
    freezedata <- T.readFile "cabal.project.freeze"
    return (parseFreeze freezedata)

solve :: Plan -> IO Plan
solve plan = do newVersions <- getVersions plan
                let newVersionMap = Map.fromList newVersions
                    mix pkg (Just ps) (Just v) = Just ps {pVersion = Just v, pVersionRange = Nothing}
                    mix pkg Nothing (Just v) = Just PS {pVersion = Just v, pVersionRange = Nothing, pExplicit = False}
                    mix pkg (Just ps) Nothing = Nothing
                    newPlan = plan { pInstall = plusMap mix (pInstall plan) newVersionMap }
                return newPlan

executePlan :: Plan -> IO (Text, String)
executePlan plan = do
  path <- prepareBuildDir plan
  withCurrentDirectory path $ do
    callProcess "cabal" ["v2-build", "-O2", "--enable-library-profiling", "--library-profiling-detail=all-functions", "--write-ghc-environment-files=always"]
    [envfile] <- glob "./.ghc.environment.*"
    let version = drop (T.length "./.ghc.environment.") envfile
    txtdata <- T.readFile envfile
    let
      cleanupFlt line
        | T.isInfixOf "Dummy-" line = Nothing
        | T.isInfixOf "dist-newstyle" line = Just (T.replace "dist-newstyle" (T.pack (path </> "dist-newstyle")) line)
        | otherwise = Just line
      cleanup = T.unlines . mapMaybe cleanupFlt . T.lines
    return (cleanup txtdata, version)

-- cabbage install
-- -p pkg
-- -p pkg-version
-- -p 'pkg < version'
-- -r pkg-to-remove
-- --minor-update pkg|all|deps
-- --major-update pkg|all|deps
-- -l local-path
-- cabbage git ...

indent :: Int -> Text -> Text
indent n txt = T.replicate n " " <> txt

explain :: Plan -> Plan -> IO ()
explain plan newplan = do
  let classify pkg (Just PS{pVersion = Just v1}) (Just PS{pVersion = Just v2})
        | v1 /= v2 = Just ("upgraded", printf "%s : %s -> %s" pkg v1 v2)
      classify pkg Nothing (Just PS{pExplicit = False, pVersion = Just v})
        = Just ("implicitly installed", printf "%s : %s" pkg v)
      classify pkg Nothing (Just PS{pExplicit = True, pVersion = Just v})
        = Just ("newly installed", printf "%s : %s" pkg v)
      classify pkg (Just PS{pExplicit = False, pVersion = Just v1}) (Just PS{pExplicit = True, pVersion = Just v2})
        = Just ("installed, previously implicit", printf "%s : %s -> %s" pkg v1 v2)
      classify pkg (Just PS{pExplicit = True}) Nothing
        = Just ("removed", printf "%s" pkg)
      classify pkg (Just PS{pExplicit = False}) Nothing
        = Just ("removed implicit dependency", printf "%s" pkg)
      classify pkg _ _ = Nothing
      classes = sort <$> Map.fromListWith (++) [(k, [v]) | (k,v) <-
                                                   toList $ plusMap classify (pInstall plan) (pInstall newplan)]

  case Map.null classes of
    False -> do
      T.putStrLn "===== Changes to be made ======"
      for_ (Map.toList classes) $ \(ty, items) -> do
        T.putStrLn (indent 1 ty)
        T.putStrLn (T.unlines $ map (indent 2 . T.pack) items)
    True -> do T.putStrLn "===== No changes ======"
               T.putStrLn "Will just build and reinstall the current registry state."

  when (not (Set.null (pLocals newplan))) $ do
    T.putStrLn "The following local packages will be (re)built:"
    T.putStrLn $ T.unlines [indent 2 p | p <- toList (pLocals newplan)]

addPackage :: Text -> Plan -> Plan
addPackage pkg plan = alterPackage pkgname go plan
  where
    (pkgname, pkgver) = parsePackage pkg
    go _ = Just PS{ pVersion = Nothing,
                    pVersionRange = Just pkgver,
                    pExplicit = True }

rmPackage :: Text -> Plan -> Plan
rmPackage pkg plan = alterPackage pkg go plan
  where go _ = Nothing

upMinor :: Plan -> Plan
upMinor plan = plan { pInstall = go <$> pInstall plan }
  where go ps@PS{pVersion = Just v} = ps {pVersion = Nothing, pVersionRange = Just (D.majorBoundVersion v)}
        go ps = ps

upMajor :: Plan -> Plan
upMajor plan = plan { pInstall = go <$> pInstall plan }
  where go ps = ps {pVersion = Nothing, pVersionRange = Just D.anyVersion}

setOutputFile :: Text -> Plan -> Plan
setOutputFile txt plan = plan { pOutputFile = Just txt }

ensure :: Text -> Plan -> Plan
ensure pkg plan = alterPackage pkg go plan
  where
    go (Just ps) = Just ps
    go (Nothing) = Just PS { pVersion = Nothing,
                             pVersionRange = Just D.anyVersion,
                             pExplicit = True }


cmdInstall :: [Plan -> Plan] -> IO ()
cmdInstall ops = do
  config <- readConfig
  let plan = confPlan config
      editplan = ensure "base" (foldl (.) id ops plan)
  newplan <- solve editplan
  explain plan newplan
  T.putStr "Continue? [Y/n] "
  hFlush stdout
  answer <- T.getLine
  when (answer == "Y" || answer == "") $ do
    (txt, ghcver) <- executePlan newplan
    case pOutputFile newplan of
      Just fpath -> do T.writeFile (T.unpack fpath) txt
                       T.putStrLn ("Wrote '" <> fpath <> "'.")
      Nothing -> writeDefaultEnvironmentFile txt ghcver
    let newConfig = planConf newplan
    when (config /= newConfig) $ do
      writeConfig newConfig
      commit

commit :: IO ()
commit = do
  args <- unwords <$> getArgs
  path <- getUserConfigDir "cabbage"
  withCurrentDirectory path $ do
    doesPathExist ".git" >>= \case
      True -> pure ()
      False -> do
        callProcess "git" ["init"]
        callProcess "git" ["add", "cabbage.conf"]
    callProcess "git" ["commit", "-am", args]

cmdGit :: [Text] -> IO ()
cmdGit args = do
  path <- getUserConfigDir "cabbage"
  withCurrentDirectory path $ do
    callProcess "git" (map T.unpack args)


main :: IO ()
main = do
  let installFlags = asum [
        addPackage <$> strArgument (metavar "PACKAGE" <>
                                    help "Install packages. Supports cabal constraint syntax (eg. lens ==4.19)"),
        rmPackage <$> strOption (short 'r' <> long "remove" <> metavar "PACKAGE" <> help "Remove packages"),
        flag' upMinor (long "minor-upgrade" <>
                       help ("Upgrade every installed package to a "
                             <> "compatible version as defined by PVP "
                             <> "(^>=)")),
        flag' upMajor (long "major-upgrade" <>
                       help ("Upgrade every installed package to the "
                             <> "newest version available (release all"
                             <> "constraints)")),
        setOutputFile <$> strOption (short 'o' <> long "output" <> metavar "FILENAME" <>
                                     help "Write output to this file, instead of the default environment in ~/.ghc")
        ]
  let cmdparse = subparser (
        command "install"
          (info (cmdInstall <$> (many installFlags <**> helper))
            (progDesc ("Install, upgrade, remove packages. " <>
                       "Options can be provided multiple times and " <>
                       "are processed from left to right.")))
          <>
          command "git"
          (info (cmdGit <$> (many (strArgument (metavar "ARGS"))) <**> helper)
            (progDesc "Run git in the config directory."))
        )
      optparser = info cmdparse fullDesc

  join $ customExecParser (prefs showHelpOnError) optparser
