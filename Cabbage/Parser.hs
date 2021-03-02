module Cabbage.Parser (
  Parser
  , parseText
  , parseFreeze
  , parsePackage
  ) where

import qualified Cabbage.Cabal as D
import           Control.Applicative
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Trifecta
import qualified Text.Trifecta as Trifecta

parseText :: Parser a -> Text -> a
parseText p txt = case parseString p mempty (T.unpack txt) of
  Success a -> a
  Failure e -> error (show (_errDoc e))

p_pkgname :: Parser Text
p_pkgname = T.pack <$> some (letter <|> digit <|> oneOf "-")

parseFreeze :: Text -> [(D.PackageName, D.Version)]
parseFreeze txt = parseText p txt
  where
    p_version = T.pack <$> some (digit <|> oneOf ".")
    p = do text "constraints:"
           spaces
           pairs <- let
             p_dep = do
               text "any."
               pkgname <- p_pkgname
               spaces
               text "=="
               version <- p_version
               optional (char ',' >> spaces)
               return [(pkgname, version)]
             p_flag = do
               p_pkgname
               spaces
               oneOf "+-"
               some (noneOf ",\n")
               optional (char ',' >> spaces)
               return []
             in fold <$> some (p_dep <|> p_flag)
           spaces
           eof
           return pairs

parsePackage :: Text -> (D.PackageName, D.VersionRange)
parsePackage pkg = parseText p pkg
  where
    p_ver = T.pack <$> some anyChar
    p = do
      pkgname <- p_pkgname <* spaces
      ver <- p_ver <|> pure D.anyVersion
      return (pkgname, ver)
