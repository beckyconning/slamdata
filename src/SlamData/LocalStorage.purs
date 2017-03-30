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

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.AVar as AVar
import DOM (DOM)
import DOM.Event.EventTarget as EventTarget
import DOM.Event.Types (EventType(..))
import DOM.HTML as DOMHTML
import DOM.HTML as HTML
import DOM.HTML.Types as DOMHTMLTypes
import DOM.HTML.Window as Window
import DOM.WebStorage.Storage as Storage
import Data.Argonaut as Argonaut
import Data.Argonaut.Core as ArgonautCore
import Data.Foreign (toForeign)
import SlamData.Prelude
import Data.Argonaut (Json, jsonParser)
import DOM.WebStorage.Event.Types as DWSET
import DOM.WebStorage.Event.StorageEvent as DWSE
import Data.Nullable as Nullable
import Control.Monad.Throw (note)

newtype Key a = Key String

derive instance keyNewtype ∷ Newtype (Key a) _

data LocalStorageF a b
  = Persist (b -> Json) (Key b) b a
  | Retrieve (Json -> Either String b) (Key b) (Either String b -> a)
  | AwaitChange (Json -> Either String b) (Key b) (b → a)
  | Remove (Key b) a

run :: forall a b eff. LocalStorageF a b -> Aff (dom :: DOM, avar :: AVAR | eff) a
run = case _ of
  Retrieve decode key k →
    k <<< (decode <=< Argonaut.jsonParser <=< note ("No key " <> unwrap key <> " in LocalStorage"))
      <<< Nullable.toMaybe
      <$> (liftEff $ Storage.getItem (unwrap key) =<< Window.localStorage =<< HTML.window)
  Persist encode key json a → do
    liftEff $ HTML.window >>= Window.localStorage >>= Storage.setItem (unwrap key) (ArgonautCore.stringify $ encode json)
    pure a
  Remove key a → do
    liftEff $ HTML.window >>= Window.localStorage >>= Storage.removeItem (unwrap key)
    pure a
  AwaitChange decode key k → do
    newValue ← AVar.makeVar
    win ← liftEff $ DOMHTMLTypes.windowToEventTarget <$> DOMHTML.window
    let listener =
          EventTarget.eventListener \event → do
            for_ (lmap show (runExcept (DWSET.readStorageEvent $ toForeign event))) \event' →
                when (Nullable.toMaybe (DWSE.key event') == Just (unwrap key))
                  $ for_ (decode =<< jsonParser =<< note "null new value" (Nullable.toMaybe (DWSE.newValue event'))) \newValue' → do
                    void $ runAff (const $ pure unit) (const $ pure unit) $ AVar.putVar newValue $ newValue'
                    liftEff $ EventTarget.removeEventListener (EventType "storage") listener false win
    liftEff $ EventTarget.addEventListener (EventType "storage") listener false win
    k <$> AVar.takeVar newValue

