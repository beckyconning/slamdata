module Utils.AVar where

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (runAff, Canceler)
import Control.Monad.Aff.AVar (AVar, AVAR)
import Control.Monad.Aff.AVar as AVar
import SlamData.Prelude

putVar ∷ forall a eff. AVar a → a → Eff (avar ∷ AVAR | eff) (Canceler (avar :: AVAR | eff))
putVar avar =
  runAff (const $ pure unit) (const $ pure unit) ∘ AVar.putVar avar

