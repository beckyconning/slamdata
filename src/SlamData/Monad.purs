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

module SlamData.Monad where

import SlamData.Prelude

import Control.Applicative.Free (FreeAp, hoistFreeAp, liftFreeAp, retractFreeAp)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff as Aff
import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Ref (readRef, writeRef)
import Control.Monad.Fork (class MonadFork, Canceler, cancelWith, fork, hoistCanceler)
import Control.Monad.Free (Free, liftF, foldFree)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Parallel (parallel, sequential)

import Data.Map as Map

import OIDC.Crypt.Types as OIDC

import Quasar.Advanced.QuasarAF as QA
import Quasar.Advanced.Types as QAT

import SlamData.Analytics as A
import SlamData.Effects (SlamDataEffects)
import SlamData.GlobalError as GE
import SlamData.Notification as N
import SlamData.SignIn (SignIn(Interactionless, Interactionful))
import SlamData.Quasar.Aff (runQuasarF)
import SlamData.Quasar.Auth (class QuasarAuthDSL)
import SlamData.Quasar.Auth.Authentication as Auth
import SlamData.Quasar.Auth.Keys as AuthKeys
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error (QError)
import SlamData.Quasar.Error as QError
import SlamData.Wiring (Wiring(..), DeckMessage(..))
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Class (class WorkspaceDSL)
import SlamData.Workspace.Deck.DeckId (DeckId)

import Unsafe.Coerce (unsafeCoerce)

import Utils (censor, passover, singletonValue)
import Utils.LocalStorage as LocalStorage

type Slam = SlamM SlamDataEffects

--------------------------------------------------------------------------------

data ForkF f x a = ForkF (f x) (Canceler f -> a)
data Fork (f :: * -> *) a

mkFork :: forall f x a. ForkF f x a → Fork f a
mkFork = unsafeCoerce

unFork :: forall f x a r. (ForkF f x a → r) → Fork f a → r
unFork = unsafeCoerce

--------------------------------------------------------------------------------

data SlamF eff a
  = Aff (Aff eff a)
  | GetAuthIdToken (Maybe OIDC.IdToken → a)
  | Quasar (QA.QuasarAFC a)
  | GetURLVarMaps (Map.Map DeckId Port.URLVarMap → a)
  | PutURLVarMaps (Map.Map DeckId Port.URLVarMap) a
  | Track A.Event a
  | Notify N.NotificationOptions a
  | Halt GE.GlobalError a
  | Par (SlamA eff a)
  | Fork (Fork (SlamM eff) a)
  | Cancel (SlamM eff a) (Canceler (SlamM eff))
  | Ask (Wiring → a)

newtype SlamM eff a = SlamM (Free (SlamF eff) a)

unSlamM ∷ ∀ eff. SlamM eff ~> Free (SlamF eff)
unSlamM (SlamM a) = a

instance functorSlamM ∷ Functor (SlamM eff) where
  map f (SlamM a) = SlamM (map f a)

instance applySlamM ∷ Apply (SlamM eff) where
  apply (SlamM a) (SlamM b) = SlamM (a <*> b)

instance applicativeSlamM ∷ Applicative (SlamM eff) where
  pure = SlamM ∘ pure

instance bindSlamM ∷ Bind (SlamM eff) where
  bind (SlamM a) f = SlamM (a >>= unSlamM ∘ f)

instance monadSlamM ∷ Monad (SlamM eff)

instance monadEffSlamM ∷ MonadEff eff (SlamM eff) where
  liftEff = SlamM ∘ liftF ∘ Aff ∘ liftEff

instance monadAffSlamM ∷ MonadAff eff (SlamM eff) where
  liftAff = SlamM ∘ liftF ∘ Aff

instance affableSlamM ∷ Affable eff (SlamM eff) where
  fromAff = SlamM ∘ liftF ∘ Aff

instance monadParSlamM ∷ Parallel (SlamA eff) (SlamM eff) where
  parallel = SlamA ∘ liftFreeAp
  sequential = SlamM ∘ liftF ∘ Par

instance monadForkSlamM ∷ MonadFork (SlamM eff) where
  fork a = SlamM $ liftF $ Fork $ mkFork $ ForkF a id
  cancelWith a c = SlamM $ liftF $ Cancel a c

instance monadAskSlamM ∷ MonadAsk Wiring (SlamM eff) where
  ask = SlamM $ liftF $ Ask id

instance quasarAuthDSLSlamM ∷ QuasarAuthDSL (SlamM eff) where
  getIdToken = SlamM $ liftF $ GetAuthIdToken id

instance quasarDSLSlamM ∷ QuasarDSL (SlamM eff) where
  liftQuasar = SlamM ∘ liftF ∘ Quasar

instance analyticsDSLSlamM ∷ A.AnalyticsDSL (SlamM eff) where
  track = SlamM ∘ liftF ∘ flip Track unit

instance notifyDSLSlamM ∷ N.NotifyDSL (SlamM eff) where
  notify notification detail timeout actionOptions =
    SlamM $ liftF $ Notify { notification, detail, timeout, actionOptions } unit

instance globalErrorDSLSlamM ∷ GE.GlobalErrorDSL (SlamM eff) where
  raiseGlobalError = SlamM ∘ liftF ∘ flip Halt unit

instance workspaceDSLSlamM ∷ WorkspaceDSL (SlamM eff) where
  getURLVarMaps = SlamM $ liftF $ GetURLVarMaps id
  putURLVarMaps = SlamM ∘ liftF ∘ flip PutURLVarMaps unit

--------------------------------------------------------------------------------

newtype SlamA eff a = SlamA (FreeAp (SlamM eff) a)

derive newtype instance functorSlamA :: Functor (SlamA eff)
derive newtype instance applySlamA :: Apply (SlamA eff)
derive newtype instance applicativeSlamA :: Applicative (SlamA eff)

--------------------------------------------------------------------------------

getIdTokenSilently ∷ SignIn → Auth.RequestIdTokenBus → Aff SlamDataEffects (Either QError Auth.EIdToken)
getIdTokenSilently signIn idTokenRequestBus = do
  case signIn of
    Interactionless →
      either (const $ getWithSingletonProviderFromQuasar) (pure ∘ Right)
        =<< getWithProviderFromLocalStorage
    Interactionful →
      getWithProviderFromLocalStorage
  where

  getWithProviderFromLocalStorage ∷ Aff SlamDataEffects (Either QError Auth.EIdToken)
  getWithProviderFromLocalStorage =
    shiftAffErrorsIntoQError $ traverse get =<< liftEff getProviderFromLocalStorage

  getWithSingletonProviderFromQuasar ∷ Aff SlamDataEffects (Either QError Auth.EIdToken)
  getWithSingletonProviderFromQuasar =
      shiftAffErrorsIntoQError $ traverse get =<< getSingletonProviderFromQuasar

  get ∷ QAT.ProviderR → Aff SlamDataEffects Auth.EIdToken
  get providerR =
    AVar.takeVar =<< passover (write providerR) =<< AVar.makeVar

  write ∷ QAT.ProviderR → AVar Auth.EIdToken → Aff SlamDataEffects Unit
  write providerR idTokenVar =
    Bus.write { idToken: idTokenVar, providerR, prompt: false, signIn } idTokenRequestBus

  getSingletonProviderFromQuasar ∷ Aff SlamDataEffects (Either QError QAT.ProviderR)
  getSingletonProviderFromQuasar =
    flip bind singletonProvider <$> runQuasarF Nothing QA.authProviders

  singletonProvider ∷ Array QAT.ProviderR → Either QError QAT.ProviderR
  singletonProvider =
    singletonValue
      (Left $ unauthorizedError $ Just noProvidersMessage)
      (const $ Left $ unauthorizedError $ Just tooManyProvidersMessage)

  getProviderFromLocalStorage ∷ Eff SlamDataEffects (Either QError QAT.ProviderR)
  getProviderFromLocalStorage =
    lmap (unauthorizedError ∘ Just) ∘ map QAT.runProvider
      <$> LocalStorage.getLocalStorage AuthKeys.providerLocalStorageKey

  noProvidersMessage ∷ String
  noProvidersMessage =
    "Quasar is not configured with any authentication providers."

  tooManyProvidersMessage ∷ String
  tooManyProvidersMessage =
    "Quasar is configured with more than one authentication providers.  sign in currently only supports configurations with a single provider."

  unauthorizedError ∷ Maybe String → QError
  unauthorizedError =
    QError.Unauthorized ∘ map QError.UnauthorizedDetails

  shiftAffErrorsIntoQError ∷ ∀ a eff. Aff eff (Either QError a) → Aff eff (Either QError a)
  shiftAffErrorsIntoQError = map (either (Left ∘ QError.Error) id) ∘ Aff.attempt

--------------------------------------------------------------------------------

runSlam :: Wiring -> Slam ~> Aff SlamDataEffects
runSlam wiring s = runReaderT (unSlam s) wiring

unSlam ∷ Slam ~> ReaderT Wiring (Aff SlamDataEffects)
unSlam = foldFree go ∘ unSlamM
  where

  go ∷ SlamF SlamDataEffects ~> ReaderT Wiring (Aff SlamDataEffects)
  go = case _ of
    Aff aff →
      lift aff
    GetAuthIdToken k → do
      Wiring { requestIdTokenBus, notify, interactionlessSignIn } ← ask
      i ← liftEff $ readRef interactionlessSignIn
      idToken ← liftAff $ censor <$> getIdTokenSilently i requestIdTokenBus
      case idToken of
        Just (Left error) →
          maybe (pure unit) (lift ∘ flip Bus.write notify) $ Auth.toNotificationOptions error
        _ →
          pure unit
      pure $ k $ maybe Nothing censor idToken
    Quasar qf → do
      Wiring { requestIdTokenBus, signInBus, notify, interactionlessSignIn } ← ask
      i ← liftEff $ readRef interactionlessSignIn
      idToken ← lift $ censor <$> getIdTokenSilently i requestIdTokenBus
      case idToken of
        Just (Left error) →
          maybe (pure unit) (lift ∘ flip Bus.write notify) $ Auth.toNotificationOptions error
        _ →
          pure unit
      lift $ runQuasarF (maybe Nothing censor idToken) qf
    GetURLVarMaps k → do
      Wiring { urlVarMaps } ← ask
      lift $ liftEff $ k <$> readRef urlVarMaps
    PutURLVarMaps urlVarMaps a → do
      Wiring wiring ← ask
      currVarMaps ← lift $ liftEff $ readRef wiring.urlVarMaps
      when (currVarMaps /= urlVarMaps) do
        lift $ liftAff $ do
          liftEff $ writeRef wiring.urlVarMaps urlVarMaps
          Bus.write URLVarMapsUpdated wiring.messaging
      pure a
    Track e a → do
      Wiring wiring ← ask
      hasIdentified ← lift $ liftEff $ readRef wiring.hasIdentified
      unless (hasIdentified) $ lift do
        liftEff $ writeRef wiring.hasIdentified true
        licensee ← runQuasarF Nothing QA.licensee
        liftEff $ for_ licensee A.identify
      liftEff $ A.trackEvent e
      pure a
    Notify no a → do
      Wiring wiring ← ask
      lift $ Bus.write no wiring.notify
      pure a
    Halt err a → do
      Wiring wiring ← ask
      lift $ Bus.write (GE.toNotificationOptions err) wiring.notify
      pure a
    Par (SlamA p) →
      sequential $ retractFreeAp $ hoistFreeAp (parallel <<< unSlam) p
    Fork f → do
      r ← ask
      goFork r f
    Cancel m c →
      cancelWith (unSlam m) (hoistCanceler unSlam c)
    Ask k →
      k <$> ask

  goFork ∷ Wiring → Fork Slam ~> ReaderT Wiring (Aff SlamDataEffects)
  goFork r = unFork \(ForkF x k) →
    k ∘ hoistCanceler (SlamM ∘ liftF ∘ Aff ∘ flip runReaderT r)
      <$> fork (unSlam x)
