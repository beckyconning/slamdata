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

module SlamData.Workspace.Card.Setups.FormInput.Labeled.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.FormInput.Labeled.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, get, put)
import Data.Array as Arr
import Data.Map as Map
import Data.Set as Set
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType)
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Behaviour as B
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.FormInput.Labeled.Model (Model, behaviour, initialState)
import SlamData.Workspace.Card.Setups.Semantics as Sem

eval
  ∷ ∀ m
  . MonadState CEM.CardState m
  ⇒ MonadThrow CE.CardError m
  ⇒ QuasarDSL m
  ⇒ Model
  → FormInputType
  → Port.Resource
  → m Port.Port
eval m formInputType resource = do
  records × axes ← BCE.analyze resource =<< get
  put (Just (CEM.Analysis { resource, axes, records}))
  case m <|> B.defaultModel behaviour m initialState{axes = axes} of
    Nothing → CE.throw "Please select axis"
    Just conf → do
      when (Arr.null records)
        $ CE.throw "The resource is empty"
      selectedValues × valueLabelMap × _ × _ ←
        Arr.foldM (foldFn conf) (Set.empty × Map.empty × 0 × 0) records
      pure
        $ Port.SetupLabeledFormInput
          { name: conf.name
          , valueLabelMap
          , selectedValues
          , formInputType
          , cursor: conf.value
          }
  where
  foldFn conf acc@(selected × vlmap × keyCount × selectedCount) record = do
    when (keyCount > FIT.maximumCountOfEntries formInputType)
      $ CE.throw
      $ "The "
      ⊕ FIT.printFormInputType formInputType
      ⊕ " form input can't take more than "
      ⊕ show (FIT.maximumCountOfEntries formInputType)
      ⊕ ". Please use 'limit' or 'group by'"
    when (selectedCount > FIT.maximumCountOfSelectedValues formInputType)
      $ CE.throw
      $ "The "
      ⊕ FIT.printFormInputType formInputType
      ⊕ " form input can't have more than "
      ⊕ show (FIT.maximumCountOfSelectedValues formInputType)
      ⊕ " selected values. Please, use other axis"
    newKeyCount × newVlmap ← case Sem.getSemantics record conf.value of
      Nothing →
        pure $ keyCount × vlmap
      Just value → do
        let
          mbNewLabel = conf.label >>= Sem.getMaybeString record
        case Map.lookup value vlmap of
          Nothing →
            pure $ (keyCount + one) × Map.insert value mbNewLabel vlmap
          Just mbExistingLabel → do
            when (mbExistingLabel ≠ mbNewLabel)
              $ CE.throw
              $ "Labels must be unique. Please, use other axis."
            pure $ keyCount × vlmap
    newSelCount × newSelected ←
      pure $ case conf.selected >>= Sem.getSemantics record of
        Nothing → selectedCount × selected
        Just value →
          if Set.member value selected
            then selectedCount × selected
            else (selectedCount + one) × Set.insert value selected
    pure $ newSelected × newVlmap × newKeyCount × newSelCount
