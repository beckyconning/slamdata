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

module SlamData.Notebook.Card.CardType
  ( CardType(..)
  , AceMode(..)
  , linkedCardType
  , autorun
  , cardName
  , cardGlyph
  , aceCardName
  , aceCardGlyph
  , aceMode
  , nextCardTypes
  , controllable
  , insertableCardTypes
  ) where

import SlamData.Prelude

import Control.Monad.Error.Class (throwError)

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Array as Arr

import Halogen.HTML.Core (HTML)
import Halogen.HTML.Indexed as HH
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as Rc

data CardType
  = Ace AceMode
  | Explore
  | Search
  | Viz
  | Chart
  | Markdown
  | JTable
  | Download
  | API
  | APIResults
  | NextAction
  | Save

insertableCardTypes ∷ Array CardType
insertableCardTypes =
  [ Ace SQLMode
  , Ace MarkdownMode
  , Explore
  , Search
  , Viz
  , Chart
  , Markdown
  , JTable
  , Download
  , API
  , APIResults
  , Save
  ]

instance eqCardType ∷ Eq CardType where
  eq (Ace m1) (Ace m2) = m1 == m2
  eq Explore Explore = true
  eq Search Search = true
  eq Viz Viz = true
  eq Chart Chart = true
  eq Markdown Markdown = true
  eq JTable JTable = true
  eq Download Download = true
  eq API API = true
  eq APIResults APIResults = true
  eq NextAction NextAction = true
  eq Save Save = true
  eq _ _ = false

data AceMode
  = MarkdownMode
  | SQLMode

instance eqAceMode ∷ Eq AceMode where
  eq MarkdownMode MarkdownMode = true
  eq SQLMode SQLMode = true
  eq _ _ = false

linkedCardType ∷ CardType → Maybe CardType
linkedCardType (Ace MarkdownMode) = Just Markdown
linkedCardType (Ace _) = Just JTable
linkedCardType Explore = Just JTable
linkedCardType Search = Just JTable
linkedCardType Viz = Just Chart
linkedCardType API = Just APIResults
linkedCardType _ = Nothing

autorun ∷ CardType → Boolean
autorun Viz = true
autorun _ = false

instance encodeJsonCardType ∷ EncodeJson CardType where
  encodeJson (Ace MarkdownMode) = encodeJson "ace-markdown"
  encodeJson (Ace SQLMode) = encodeJson "ace-sql"
  encodeJson Explore = encodeJson "explore"
  encodeJson Search = encodeJson "search"
  encodeJson Viz = encodeJson "viz"
  encodeJson Chart = encodeJson "chart"
  encodeJson Markdown = encodeJson "markdown"
  encodeJson JTable = encodeJson "jtable"
  encodeJson Download = encodeJson "download"
  encodeJson API = encodeJson "api"
  encodeJson APIResults = encodeJson "api-results"
  encodeJson NextAction = encodeJson "next-action"
  encodeJson Save = encodeJson "save"

instance decodeJsonCardType ∷ DecodeJson CardType where
  decodeJson json = do
    str ← decodeJson json
    case str of
      "ace-markdown" → pure $ Ace MarkdownMode
      "ace-sql" → pure $ Ace SQLMode
      "explore" → pure Explore
      "search" → pure Search
      "viz" → pure Viz
      "chart" → pure Chart
      "markdown" → pure Markdown
      "jtable" → pure JTable
      "download" → pure Download
      "api" → pure API
      "api-results" → pure APIResults
      "next-action" → pure NextAction
      "save" → pure Save
      name → throwError $ "unknown card type '" ⊕ name ⊕ "'"

cardName ∷ CardType → String
cardName (Ace at) = aceCardName at
cardName Explore = "Explore"
cardName Search = "Search"
cardName Viz = "Visualize"
cardName Chart = "Chart"
cardName Markdown = "Form"
cardName JTable = "Table"
cardName Download = "Download"
cardName API = "API"
cardName APIResults = "API Results"
cardName NextAction = "Next Action"
cardName Save = "Save"

cardGlyph ∷ ∀ s f. CardType → HTML s f
cardGlyph (Ace at) = glyph $ aceCardGlyph at
cardGlyph Explore = glyph B.glyphiconEyeOpen
cardGlyph Search = glyph B.glyphiconSearch
cardGlyph Viz = glyph B.glyphiconPicture
cardGlyph Download = glyph B.glyphiconDownloadAlt
cardGlyph API = glyph B.glyphiconOpenFile
cardGlyph APIResults = glyph B.glyphiconTasks
cardGlyph Chart =
  HH.div
    [ HP.classes [ Rc.glyphImage, Rc.chartGlyph ]
    ] [ ]
cardGlyph Markdown =
  HH.div
    [ HP.classes [ Rc.glyphImage, Rc.codeGlyph ]
    ] [ ]
cardGlyph JTable = glyph B.glyphiconThList
cardGlyph NextAction = glyph B.glyphiconStop
cardGlyph Save = glyph B.glyphiconFloppyDisk

aceCardName ∷ AceMode → String
aceCardName MarkdownMode = "Markdown"
aceCardName SQLMode = "Query"

aceCardGlyph ∷ AceMode → HH.ClassName
aceCardGlyph MarkdownMode = B.glyphiconEdit
aceCardGlyph SQLMode = B.glyphiconQuestionSign

aceMode ∷ AceMode → String
aceMode MarkdownMode = "ace/mode/markdown"
aceMode SQLMode = "ace/mode/sql"

nextCardTypes ∷ Maybe CardType → Array CardType
nextCardTypes Nothing =
  [
    Ace SQLMode
  , Ace MarkdownMode
  , Explore
  , API
  ]
nextCardTypes (Just ct) = case ct of
  Explore → dataSourceCards
  Search → dataSourceCards
  Ace SQLMode → dataSourceCards
  Viz → [ Chart ]
  API → [ APIResults ]
  Ace MarkdownMode → [ Markdown ]
  Markdown → [ Ace SQLMode ]
  JTable → dataSourceOutput `Arr.snoc` Save
  Download → [ ]
  APIResults →  [ Ace SQLMode ]
  Chart → [ ]
  NextAction → [ ]
  Save → dataSourceOutput `Arr.snoc` JTable

  where
  dataSourceOutput =
    [
      Download, Search, Ace SQLMode, Viz
    ]
  dataSourceCards =
    (dataSourceOutput `Arr.snoc` JTable) `Arr.snoc` Save


controllable ∷ CardType → Boolean
controllable NextAction = false
controllable _ = true