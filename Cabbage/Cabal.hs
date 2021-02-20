{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}

module Cabbage.Cabal where

import           Data.Text (Text)
import qualified Data.Text as T

type Version = Text
type VersionRange = Text
type PackageName = Text

data Dependency = Dependency {
  depPackage :: PackageName,
  depVersion :: VersionRange
  }

anyVersionDep :: PackageName -> Dependency
anyVersionDep pkgname = Dependency pkgname anyVersion

anyVersion :: VersionRange
anyVersion = "-any"

thisVersion :: Version -> VersionRange
thisVersion v = "==" <> v

majorBoundVersion :: Version -> VersionRange
majorBoundVersion v = "^>=" <> v

gtVersion :: Version -> VersionRange
gtVersion v = ">=" <> v

versionOfRange :: VersionRange -> Maybe Version
versionOfRange vrange
  | T.isPrefixOf "==" vrange = Just (head $ T.words $ T.drop 2 vrange)
  | otherwise = Nothing

parseDep :: Text -> Dependency
parseDep txt = case T.breakOn " " txt of
  (name, ver) -> Dependency name (T.drop 1 ver)

prettyShow = T.unpack

intersectVersionRanges :: VersionRange -> VersionRange -> VersionRange
intersectVersionRanges a b = a <> " && " <> b

showDep :: Dependency -> Text
showDep Dependency{..} = depPackage <> " " <> depVersion

indent :: Int -> Text -> Text
indent n = T.unlines . map (T.replicate n " " <>) . T.lines

makeDummyPackage :: [Dependency] -> Text
makeDummyPackage deps = pkgText
  where
    pkgText = T.unlines [
      "cabal-version: 2.4",
      "name: Dummy",
      "version: 0.1.0.0",
      "license: MIT",
      "",
      "library",
      "    default-language: Haskell2010",
      "    build-depends:",
      deplines]
    deplines = indent 8 $ T.intercalate ",\n" [showDep d | d <- deps]
