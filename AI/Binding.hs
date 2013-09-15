{-# LANGUAGE RecordWildCards #-}
module AI.Binding where

import qualified Data.HashMap.Strict as HM
import Data.Hashable

data Bind k v = Bind
  { methods :: HM.HashMap k (k -> v)
  , ifMissing :: k -> v
  , super :: Maybe (Bind k v)
  }

instance Show (Bind k v) where
  show _ = "<bindings>"

makeMethodMap :: (Eq k, Hashable k) => [(k, k -> v)] -> HM.HashMap k (k -> v)
makeMethodMap = HM.fromList

getMethod :: (Eq k, Hashable k) => Bind k v -> k -> v
getMethod Bind{..} k = HM.lookupDefault ifMissing k methods k
