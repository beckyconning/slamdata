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

module SlamData.Workspace.Deck.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cpL, cpR)

import SlamData.Workspace.Card.Component (CardQueryP, CardStateP)
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Deck.BackSide.Component as Back


newtype CardSlot = CardSlot CardId

derive instance genericCardSlot :: Generic CardSlot
instance eqCardSlot :: Eq CardSlot where eq = gEq
instance ordCardSlot :: Ord CardSlot where compare = gCompare

type BackSideSlot = Unit

type ChildSlot =
  Either CardSlot BackSideSlot

type ChildQuery =
  Coproduct CardQueryP Back.Query

type ChildState =
  Either CardStateP Back.State


cpCard
  ∷ ChildPath
      CardStateP ChildState
      CardQueryP ChildQuery
      CardSlot ChildSlot
cpCard = cpL

cpBackSide
  ∷ ChildPath
      Back.State ChildState
      Back.Query ChildQuery
      BackSideSlot ChildSlot
cpBackSide = cpR