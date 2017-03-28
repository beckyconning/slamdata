module SlamData.LocalStorage.Class
  ( module SlamData.LocalStorage.Class
  , module SlamData.LocalStorage
  ) where

import SlamData.Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, encodeJson, decodeJson)
import Halogen (HalogenM)
import SlamData.LocalStorage (Key(..), LocalStorageF(..), run)
import DOM (DOM)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Type.Row.Effect.Equality (class EffectRowEquals, effFrom)

class LocalStorageDSL m where
  retrieve ∷ forall a. DecodeJson a ⇒ Key a → m (Either String a)
  persist ∷ forall a. EncodeJson a ⇒ Key a → a → m Unit
  remove ∷ forall a. EncodeJson a ⇒ Key a → m Unit

instance localStorageDSLMaybeT ∷ (Monad m, LocalStorageDSL m) ⇒ LocalStorageDSL (MaybeT m) where
  retrieve = lift ∘ retrieve
  persist k = lift ∘ persist k
  remove = lift ∘ remove

instance localStorageDSLExceptT ∷ (Monad m, LocalStorageDSL m) ⇒ LocalStorageDSL (ExceptT e m) where
  retrieve = lift ∘ retrieve
  persist k = lift ∘ persist k
  remove = lift ∘ remove

instance localStorageDSLHalogenM ∷ (Monad m, LocalStorageDSL m) ⇒ LocalStorageDSL (HalogenM s f g p o m) where
  retrieve = lift ∘ retrieve
  persist k = lift ∘ persist k
  remove = lift ∘ remove

instance localStorageDSLEff ∷ EffectRowEquals eff (dom ∷ DOM | eff') ⇒ LocalStorageDSL (Eff eff) where
  retrieve k = effFrom $ run $ Retrieve decodeJson k id
  persist k x = effFrom $ run $ Persist encodeJson k x unit
  remove k = effFrom $ run $ Remove k unit

instance localStorageDSLAff ∷ EffectRowEquals eff (dom ∷ DOM | eff') ⇒ LocalStorageDSL (Aff eff) where
  retrieve = liftEff ∘ retrieve
  persist k = liftEff ∘ persist k
  remove = liftEff ∘ remove
