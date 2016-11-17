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

module SlamData.Quasar.Auth.Store where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Either (Either)
import DOM (DOM)
import OIDC.Crypt.Types as OIDCT
import Quasar.Advanced.Types as QAT
import SlamData.Quasar.Auth.Keys as AuthKeys
import SlamData.SignIn (SignIn)
import Utils.LocalStorage as LS

storeIdToken ∷ ∀ e. SignIn → Either String OIDCT.IdToken → Eff (dom ∷ DOM | e) Unit
storeIdToken signIn idToken =
  LS.setLocalStorage
    AuthKeys.idTokenLocalStorageKey
    $ OIDCT.runIdToken <$> idToken

storeProvider ∷ ∀ e. SignIn → QAT.Provider → Eff (dom ∷ DOM | e) Unit
storeProvider signIn =
  LS.setLocalStorage AuthKeys.providerLocalStorageKey

clearProvider ∷ ∀ e. SignIn → Eff (dom ∷ DOM | e) Unit
clearProvider signIn =
  LS.removeLocalStorage AuthKeys.providerLocalStorageKey

storeKeyString ∷ ∀ e. SignIn → OIDCT.KeyString → Eff (dom ∷ DOM |e) Unit
storeKeyString signIn (OIDCT.KeyString ks) =
  LS.setLocalStorage
    AuthKeys.keyStringLocalStorageKey
    ks

storeUnhashedNonce ∷ ∀ e. SignIn → OIDCT.UnhashedNonce → Eff (dom ∷ DOM |e) Unit
storeUnhashedNonce signIn (OIDCT.UnhashedNonce n) =
  LS.setLocalStorage
    AuthKeys.nonceLocalStorageKey
    n

clearIdToken ∷ ∀ e. SignIn → Eff (dom ∷ DOM |e) Unit
clearIdToken signIn =
  LS.removeLocalStorage AuthKeys.idTokenLocalStorageKey

clearUnhashedNonce ∷ ∀ e. SignIn → Eff (dom ∷ DOM |e) Unit
clearUnhashedNonce signIn =
  LS.removeLocalStorage AuthKeys.nonceLocalStorageKey
