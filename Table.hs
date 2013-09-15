module Table
  ( Tabular
  , readRecord
  , readField
  , copyField
  , readTSVFile
  , readTSVString
  , readTable
  ) where

import Text.Parsec
import Text.Parsec.Text

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Arrow ((|||), left)
import Control.Monad.Reader
import Control.Monad.Error
import Text.Read (readEither)

type Key = String
type Value = String
type Row = [(Key, Value)]
type Table = [Row]

type RecordReader = ReaderT Row (Either String)

class Tabular r where
  readRecord :: RecordReader r

-- TODO Less nonsensical error handling for user code
readTSVFile :: Tabular r => String -> IO [r]
readTSVFile file = do
  text <- TIO.readFile file
  error ||| return $ readTable text file

readTSVString :: Tabular r => String -> [r]
readTSVString text = error ||| id $ readTable (T.pack text) ""

readTable :: Tabular r => T.Text -> String -> Either String [r]
readTable text source = do
  rows <- show `left` parse tsvTable source text
  mapM (runReaderT readRecord) rows

readValue :: Read v => String -> RecordReader v
readValue = (throwError ||| return) . readEither

readField :: Read v => String -> RecordReader v
readField k = copyField k >>= readValue

copyField :: String -> RecordReader String
copyField k = asks (lookup k) ??? "No such key: " ++ k

(???) :: (MonadError e m) => m (Maybe b) -> e -> m b
f ??? e = do
  r <- f
  case r of
    Just v -> return v
    Nothing -> throwError e

infix 0 ???

tsvTable :: Parser Table
tsvTable = do
  header <- tsvRow
  _ <- newline
  tsvRecord header `sepEndBy` newline

tsvRow :: Parser [Value]
tsvRow = tsvField `sepBy` tab

tsvZip :: [Key] -> [Value] -> Parser Row
tsvZip (k:ks) (v:vs) = ((k, v) :) `fmap` tsvZip ks vs
tsvZip [] [] = return []
tsvZip [] _ = fail "too many fields"
tsvZip _ [] = fail "not enough fields"

tsvRecord :: [Key] -> Parser Row
tsvRecord header = tsvRow >>= tsvZip header

tsvField :: Parser String
tsvField = many . noneOf $ "\t\r\n"
