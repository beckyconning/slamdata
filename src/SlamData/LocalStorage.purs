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

module SlamData.LocalStorage where

import SlamData.Prelude
import Data.Argonaut (Json)
import Control.Monad.Throw (note)
import Data.Nullable as Nullable
import Data.Argonaut as Argonaut
import Data.Argonaut.Core as ArgonautCore
import DOM (DOM)
import DOM.WebStorage.Storage as Storage
import DOM.HTML.Window as Window
import DOM.HTML as HTML
import Control.Monad.Eff (Eff)

newtype Key a = Key String

derive instance keyNewtype ∷ Newtype (Key a) _

data LocalStorageF a b
  = Persist (b -> Json) (Key b) b a
  | Retrieve (Json -> Either String b) (Key b) (Either String b -> a)
  | Remove (Key b) a

run :: forall a b eff. LocalStorageF a b -> Eff (dom :: DOM | eff) a
run = case _ of
  Retrieve decode key k →
    k <<< (decode <=< Argonaut.jsonParser <=< note ("No key " <> unwrap key <> " in LocalStorage"))
      <<< Nullable.toMaybe
      <$> (Storage.getItem (unwrap key) =<< Window.localStorage =<< HTML.window)
  Persist encode key json a → do
    HTML.window >>= Window.localStorage >>= Storage.setItem (unwrap key) (ArgonautCore.stringify $ encode json)
    pure a
  Remove key a → do
    HTML.window >>= Window.localStorage >>= Storage.removeItem (unwrap key)
    pure a

