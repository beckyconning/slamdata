module Utils.StorageEvent where

import DOM.Event.Types (Event)
import Data.Foreign (toForeign)
import SlamData.Prelude
import DOM.WebStorage.Event.Types as DWSET
import DOM.WebStorage.Event.StorageEvent as DWSE
import Data.Nullable as Nullable
import Control.Monad.Throw (note)
import Data.Argonaut (class DecodeJson, Json, decodeJson, jsonParser)

read ∷ Event → Either String DWSET.StorageEvent
read =
  lmap show ∘ runExcept ∘ DWSET.readStorageEvent ∘ toForeign

keyEq ∷ String → DWSET.StorageEvent → Boolean
keyEq key event =
  Nullable.toMaybe (DWSE.key event) == Just key

decodeNewValue' ∷ forall a. (Json -> Either String a) → DWSET.StorageEvent → Either String a
decodeNewValue' decode event =
  decode =<< jsonParser =<< note "null new value" (Nullable.toMaybe (DWSE.newValue event))

decodeNewValue ∷ forall a. DecodeJson a ⇒ DWSET.StorageEvent → Either String a
decodeNewValue =
  decodeNewValue' decodeJson

