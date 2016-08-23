{-
Copyright 2016 SlamData, Inc.

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

module Utils.At where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff as Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER, IntervalId)
import Control.Monad.Eff.Timer as Timer
import Control.Monad.Eff.Now as Now
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Ref as Ref
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Time.Duration (Milliseconds)
import Data.Time.Duration as Duration
import DOM (DOM)
import SlamData.Prelude

setInterval
  ∷ ∀ eff
  . Int
  → (Maybe IntervalId → Eff (ref ∷ REF, timer ∷ TIMER | eff) Unit)
  → Eff (ref ∷ REF, timer ∷ TIMER | eff) IntervalId
setInterval ms action = do
  var ← Ref.newRef Nothing
  intervalId ← Timer.setInterval ms (action =<< Ref.readRef var)
  Ref.writeRef var $ Just intervalId
  pure intervalId

type AtEffects eff = (timer ∷ TIMER, ref ∷ REF, now ∷ NOW, dom ∷ DOM | eff)

at ∷ ∀ eff a. Instant → Int → Eff (AtEffects eff) a → Aff (AtEffects eff) a
at instant accuracy action =
  Aff.makeAff' \_ success → do
    cancellable ← Ref.newRef true
    interval ← setInterval accuracy \maybeIntervalForAction → do
      case maybeIntervalForAction of
        Nothing → pure unit
        Just intervalForAction → do
          now ← Now.now
          if now >= instant
            then
              (Timer.clearInterval intervalForAction)
                *> (Ref.writeRef cancellable false)
                *> (success =<< liftEff action)
            else pure unit
    pure $ Aff.Canceler $ const $ liftEff $ Timer.clearInterval interval *> Ref.readRef cancellable

laterThan ∷ Milliseconds → Instant → Maybe Instant
laterThan ms instant =
  Instant.instant
    $ Duration.Milliseconds
    $ Duration.unMilliseconds (Instant.unInstant instant)
    + Duration.unMilliseconds ms
