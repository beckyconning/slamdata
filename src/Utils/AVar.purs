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

module Utils.AVar where

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (runAff, Canceler)
import Control.Monad.Aff.AVar (AVar, AVAR)
import Control.Monad.Aff.AVar as AVar
import SlamData.Prelude

putVar ∷ forall a eff. AVar a → a → Eff (avar ∷ AVAR | eff) (Canceler (avar :: AVAR | eff))
putVar avar =
  runAff (const $ pure unit) (const $ pure unit) ∘ AVar.putVar avar

