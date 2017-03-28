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

