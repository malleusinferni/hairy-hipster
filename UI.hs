module UI where

import System.IO (stdout, hFlush)

prompt :: String -> IO String
prompt str = do
  putStr str
  hFlush stdout
  getLine

promptYN str = do
  r <- prompt str
  case r of
    [] -> return True
    ('y':_) -> return True
    ('Y':_) -> return True
    _ -> return False
