{-# LANGUAGE OverloadedStrings #-}

module HostParser(HostFileLine(..), hostFileLine) where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Text                      ( Text )
import           Data.Either                    ( Either )
import qualified Data.Text                     as T
import           Data.Maybe
import           Data.Void

import           Data.List                      ( intercalate )
import           Data.Either.Combinators        ( mapLeft )


type Parser = Parsec Void Text

type Ip = Text
type Hostname = Text
type Comment = Text

data HostFileLine = Blank | Comment Text | Host Ip Hostname deriving (Show)

commentLine :: Parser HostFileLine
commentLine = Comment <$> (space *> char '#' *> takeRest)

blankLine :: Parser HostFileLine
blankLine = Blank <$ (space *> eof)

hostLine :: Parser HostFileLine
hostLine = do
  space
  ip' <- ip
  space1
  hostname <- some (alphaNumChar <|> char '-') `sepBy1` dot
  return $ Host ip' (T.pack $ intercalate "." hostname)

dot = char '.'

ip :: Parser Ip
ip = do
  a <- count' 1 3 digitChar
  dot
  b <- count' 1 3 digitChar
  dot
  c <- count' 1 3 digitChar
  dot
  d <- count' 1 3 digitChar
  return $ T.pack $ intercalate "." [a, b, c, d]


hostFileLine :: Text -> Either (ParseErrorBundle Text Void) HostFileLine
hostFileLine =
  parse (try blankLine <|> try commentLine <|> try hostLine) ""
