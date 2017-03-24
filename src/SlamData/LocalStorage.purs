module SlamData.LocalStorage where

import SlamData.Prelude
import Data.Argonaut (Json)

data LocalStorageF a
  = Persist String Json a
  | Retrieve String (Either String Json -> a)

