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

module SlamData.Notebook (main) where

import SlamData.Prelude

import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Ace.Config as AceConfig

import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)

import Halogen (Driver, runUI, parentState)
import Halogen.Util (runHalogenAff, awaitBody)

import SlamData.Config as Config
import SlamData.FileSystem.Routing (parentURL)
import SlamData.Notebook.Action (Action(..), toAccessType)
import SlamData.Notebook.Card.CardId as CID
import SlamData.Notebook.Card.Port as Port
import SlamData.Notebook.Component as Draftboard
import SlamData.Notebook.Deck.Component as Deck
import SlamData.Effects (SlamDataRawEffects, SlamDataEffects)
import SlamData.Notebook.Rename.Component as Rename
import SlamData.Notebook.Routing (Routes(..), routing)
import SlamData.Notebook.StyleLoader as StyleLoader

import Utils.Path as UP

main ∷ Eff SlamDataEffects Unit
main = do
  AceConfig.set AceConfig.basePath (Config.baseUrl ⊕ "js/ace")
  AceConfig.set AceConfig.modePath (Config.baseUrl ⊕ "js/ace")
  AceConfig.set AceConfig.themePath (Config.baseUrl ⊕ "js/ace")
  browserFeatures ← detectBrowserFeatures
  runHalogenAff do
    let st = parentState $ Draftboard.initialState { browserFeatures: browserFeatures }
    driver ← runUI Draftboard.comp st =<< awaitBody
    forkAff (routeSignal driver)
  StyleLoader.loadStyles

routeSignal
  ∷ Driver Draftboard.QueryP SlamDataRawEffects
  → Aff SlamDataEffects Unit
routeSignal driver = do
  Tuple _ route ← Routing.matchesAff' UP.decodeURIPath routing
  case route of
    CardRoute res cardId accessType varMap →
      notebook res (Load accessType) (Just cardId) varMap
    NotebookRoute res action varMap → notebook res action Nothing varMap
    ExploreRoute res → explore res

  where

  explore ∷ UP.FilePath → Aff SlamDataEffects Unit
  explore path = do
    fs ← liftEff detectBrowserFeatures
    driver $ Draftboard.toDeck $ Deck.ExploreFile fs path
    driver $ Draftboard.toDraftboard $ Draftboard.SetParentHref
      $ parentURL $ Right path
    driver $ Draftboard.toRename $ Rename.SetText $ Config.newNotebookName

  notebook
    ∷ UP.DirPath
    → Action
    → Maybe CID.CardId
    → Port.VarMap
    → Aff SlamDataEffects Unit
  notebook path action viewing varMap = do
    let name = UP.getNameStr $ Left path
        accessType = toAccessType action
    currentPath ← driver $ Draftboard.fromDeck Deck.GetNotebookPath
    currentVarMap ← driver $ Draftboard.fromDeck Deck.GetGlobalVarMap
    currentViewing ← driver $ Draftboard.fromDraftboard Draftboard.GetViewingCard
    currentAccessType ← driver $ Draftboard.fromDraftboard Draftboard.GetAccessType

    when (currentPath ≠ pure path) do
      features ← liftEff detectBrowserFeatures
      if action ≡ New
        then driver $ Draftboard.toDeck $ Deck.Reset features path
        else driver $ Draftboard.toDeck $ Deck.LoadNotebook features path

    driver $ Draftboard.toRename $ Rename.SetText $ UP.dropNotebookExt name
    driver $ Draftboard.toDraftboard $ Draftboard.SetViewingCard viewing
    driver $ Draftboard.toDraftboard $ Draftboard.SetAccessType accessType
    driver $ Draftboard.toDeck $ Deck.SetGlobalVarMap varMap
    driver $ Draftboard.toDraftboard $ Draftboard.SetParentHref
      $ parentURL $ Left path
