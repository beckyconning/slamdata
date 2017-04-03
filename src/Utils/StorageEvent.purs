{-
Copyright 2017 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

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

