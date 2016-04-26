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

module SlamData.Notebook.Deck.Component.Query
  ( Query(..)
  , QueryP
  ) where

import SlamData.Prelude

import Data.BrowserFeatures as BF
import DOM.HTML.Types (HTMLElement)

import Halogen (ChildF)
import Halogen.HTML.Events.Types (Event, MouseEvent)

import SlamData.Notebook.AccessType as AT
import SlamData.Notebook.Card.CardId as CID
import SlamData.Notebook.Card.CardType as CT
import SlamData.Notebook.Card.Port.VarMap as Port
import SlamData.Notebook.Deck.Component.ChildSlot (ChildSlot, ChildQuery)

import Utils.Path as UP

-- | `GetNotebookPath` returns the notebook's path, constructed using
-- | `notebookPath`.
data Query a
  = AddCard CT.CardType a
  | RunActiveCard a
  | RunPendingCards a
  | GetNotebookPath (Maybe UP.DirPath → a)
  | SetViewingCard (Maybe CID.CardId) a
  | SetName String a
  | SetAccessType AT.AccessType a
  | ExploreFile BF.BrowserFeatures UP.FilePath a
  | Publish a
  | LoadNotebook BF.BrowserFeatures UP.DirPath a
  | SaveNotebook a
  | Reset BF.BrowserFeatures UP.DirPath a
  | GetGlobalVarMap (Port.VarMap → a)
  | SetGlobalVarMap Port.VarMap a
  | FindCardParent CID.CardId (Maybe CID.CardId → a)
  | GetCardType CID.CardId (Maybe CT.CardType → a)
  | FlipDeck a
  | GetActiveCardId (Maybe CID.CardId → a)
  | StartSliding (Event MouseEvent) a
  | StopSlidingAndSnap (Event MouseEvent) a
  | UpdateSliderPosition (Event MouseEvent) a
  | SetNextActionCardElement (Maybe HTMLElement) a
  | StopSliderTransition a

type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)
