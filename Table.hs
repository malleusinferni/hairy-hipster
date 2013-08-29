{-# LANGUAGE RecordWildCards #-}
module Table
  ( Tabular
  , readRecord
  , readField
  , readTSVFile
  , readTSVString
  , readTable
  ) where

import Text.Parsec
import Text.Parsec.Text

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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

readTSVFile :: Tabular r => String -> IO [r]
readTSVFile file = do
  text <- TIO.readFile file
  either error return $ readTable text file

readTSVString :: Tabular r => String -> [r]
readTSVString text = either error id $ readTable (T.pack text) ""

readTable :: Tabular r => T.Text -> String -> Either String [r]
readTable text source = do
  rows <- show `onLeft` parse tsvTable source text
  mapM (runReaderT readRecord) rows

onLeft :: (a -> b) -> Either a r -> Either b r
onLeft f (Left v) = Left (f v)
onLeft _ (Right v) = Right v -- rebind

readValue :: Read v => String -> RecordReader v
readValue = either throwError return . readEither

readField :: Read v => String -> RecordReader v
readField k = do
  v <- asks (lookup k)
  case v of
    Just v' -> readValue v'
    Nothing -> throwError $ "No such key: " ++ k

tsvTable :: Parser [Row]
tsvTable = do
  header <- tsvRow
  newline
  tsvRecord header `sepEndBy` newline

tsvRow :: Parser [Value]
tsvRow = tsvField `sepBy` tab

tsvZip :: [Key] -> [Value] -> Parser Row
tsvZip (k:ks) (v:vs) = ((k, v) :) `fmap` tsvZip ks vs
tsvZip [] [] = return []
tsvZip [] _ = fail "too many fields"
tsvZip _ [] = fail "not enough fields"

tsvRecord :: [Key] -> Parser Row
tsvRecord header = do
  row <- tsvRow
  tsvZip header row

tsvField :: Parser String
tsvField = many . noneOf $ "\t\r\n"
