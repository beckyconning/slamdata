module SlamData.LocalStorage.Class where

import SlamData.Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Halogen (HalogenM)

newtype Key a = Key String

class LocalStorageDSL m where
  retrieve ∷ forall a. DecodeJson a ⇒ Key a → m (Either String a)
  persist ∷ forall a. EncodeJson a ⇒ Key a → a → m Unit

instance localStorageDSLMaybeT ∷ (Monad m, LocalStorageDSL m) ⇒ LocalStorageDSL (MaybeT m) where
  retrieve = lift ∘ retrieve
  persist k = lift ∘ persist k

instance localStorageDSLExceptT ∷ (Monad m, LocalStorageDSL m) ⇒ LocalStorageDSL (ExceptT e m) where
  retrieve = lift ∘ retrieve
  persist k = lift ∘ persist k

instance localStorageDSLHalogenM ∷ (Monad m, LocalStorageDSL m) ⇒ LocalStorageDSL (HalogenM s f g p o m) where
  retrieve = lift ∘ retrieve
  persist k = lift ∘ persist k

instance keyNewtype ∷ Newtype (Key a) String where
  unwrap (Key s) = s
  wrap = Key
