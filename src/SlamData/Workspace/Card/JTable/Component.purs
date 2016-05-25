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

module SlamData.Workspace.Card.JTable.Component
  ( jtableComponent
  , queryShouldRun
  , module SlamData.Workspace.Card.JTable.Component.Query
  , module JTS
  ) where

import SlamData.Prelude

import Control.Apply (lift3)
import Control.Monad.Aff.Free (fromEff)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class (throwError)

import Data.Argonaut.Core as JSON
import Data.Int as Int
import Data.Lens ((.~), (?~))

import DOM.HTML.Types (HTMLElement)

import Halogen as H

import SlamData.Effects (Slam)
import SlamData.Quasar.Query as Quasar
import SlamData.Workspace.Card.CardType as Ct
import SlamData.Workspace.Card.Common.EvalQuery as CEQ
import SlamData.Workspace.Card.Component (CardQueryP, CardStateP, makeCardComponent, makeQueryPrism, _JTableState, _JTableQuery)
import SlamData.Workspace.Card.JTable.Component.Query (QueryP, PageStep(..), Query(..))
import SlamData.Workspace.Card.JTable.Component.Render as JTR
import SlamData.Workspace.Card.JTable.Component.State as JTS
import SlamData.Workspace.Card.JTable.Model as Model
import SlamData.Workspace.Card.Port as Port

import Utils.DOM as DOMUtils

type JTableDSL = H.ComponentDSL JTS.State QueryP Slam

type Dimensions = { width ∷ Number, height ∷ Number }

jtableComponent ∷ H.Component CardStateP CardQueryP Slam
jtableComponent =
  makeCardComponent
    { cardType: Ct.JTable
    , component: H.lifecycleComponent
                   { render: JTR.render
                   , eval: coproduct evalCard evalJTable
                   , initializer: Just $ right $ H.action UpdateLevelOfDetail
                   , finalizer: Nothing
                   }
    , initialState: JTS.initialState
    , _State: _JTableState
    , _Query: makeQueryPrism _JTableQuery
    }

queryShouldRun ∷ ∀ a. QueryP a → Boolean
queryShouldRun = coproduct (const false) pred
  where
  pred (StepPage _ _) = true
  pred (ChangePageSize _ _) = true
  pred _ = false

-- | Evaluates generic card queries.
evalCard ∷ Natural CEQ.CardEvalQuery JTableDSL
evalCard (CEQ.NotifyRunCard next) = updateLevelOfDetail $> next
evalCard (CEQ.NotifyStopCard next) = pure next
evalCard (CEQ.EvalCard value k) = do
  k <$> CEQ.runCardEvalT (runTable value.inputPort)
evalCard (CEQ.SetupCard _ next) = pure next
evalCard (CEQ.Save k) =
  pure ∘ k =<< H.gets (Model.encode ∘ JTS.toModel)
evalCard (CEQ.Load json next) = do
  either (const (pure unit)) H.set $ JTS.fromModel <$> Model.decode json
  updateLevelOfDetail
  pure next
evalCard (CEQ.SetCanceler _ next) = pure next
evalCard (CEQ.UpdateCardElementAndDimensions cardElement cardDimensions next) =
  H.modify ((JTS._cardElement .~ Just cardElement) ∘ (JTS._cardDimensions .~ Just cardDimensions))
    *> updateLevelOfDetail
    $> next

updateLevelOfDetail :: JTableDSL Unit
updateLevelOfDetail = (H.modify <<< (JTS._levelOfDetail .~ _) =<< getLevelOfDetail)

getTableBoundingClientRect ∷ HTMLElement → JTableDSL (Maybe DOMUtils.DOMRect)
getTableBoundingClientRect =
  fromEff <<< DOMUtils.querySelectorBoundingClientRect "table"

getNumberOfRows ∷ JTableDSL Int
getNumberOfRows =
  JTS.numberOfRows <$> H.get

getNumberOfRowsWidthPx ∷ JTableDSL Number
getNumberOfRowsWidthPx =
  fromEff <<< DOMUtils.getTextWidth JTR.numberOfRowsFont <<< show =<< getNumberOfRows

getNumberOfRowsHeightPx ∷ JTableDSL Number
getNumberOfRowsHeightPx =
  fromEff $ DOMUtils.remToPx JTR.numberOfRowsFontSizeRem

getNumberOfRowsDimensionsPx ∷ JTableDSL Dimensions
getNumberOfRowsDimensionsPx =
  { width: _, height: _ } <$> getNumberOfRowsWidthPx <*> getNumberOfRowsHeightPx

getLevelOfDetail ∷ JTableDSL (Maybe JTS.LevelOfDetail)
getLevelOfDetail =
  (lift3 levelOfDetail)
    <$> H.gets _.cardDimensions
    <*> (maybe (pure Nothing) getTableBoundingClientRect =<< H.gets _.cardElement)
    <*> (Just <$> getNumberOfRowsDimensionsPx)

numberOfRowsFitsInCard ∷ Dimensions → Dimensions → Boolean
numberOfRowsFitsInCard cardDimensions numberOfRowsDimensions =
  cardDimensions.width >= numberOfRowsDimensions.width
    && cardDimensions.height >= numberOfRowsDimensions.height

levelOfDetail ∷ Dimensions → DOMUtils.DOMRect → Dimensions → JTS.LevelOfDetail
levelOfDetail cardDimensions tableDimensions numberOfRowsDimensions
  | cardDimensions.width >= tableDimensions.width = JTS.Table
  | numberOfRowsFitsInCard cardDimensions numberOfRowsDimensions = JTS.NumberOfRows
  | otherwise = JTS.TooSmall

runTable
  ∷ Maybe Port.Port
  → CEQ.CardEvalT (H.ComponentDSL JTS.State QueryP Slam) (Maybe Port.Port)
runTable inputPort = case inputPort of
  Just (Port.TaggedResource { tag, resource }) → do
    oldInput ← lift $ H.gets _.input
    when (((oldInput <#> _.resource) ≠ pure resource) || ((oldInput >>= _.tag) ≠ tag))
      $ lift $ resetState

    size ←
      lift (Quasar.count resource)
        >>= either (throwError ∘ Exn.message) pure

    lift $ H.modify $ JTS._input ?~ { resource, size, tag }
    p ← lift $ H.gets JTS.pendingPageInfo

    items ←
      lift (Quasar.sample resource ((p.page - 1) * p.pageSize) p.pageSize)
        >>= either (throwError ∘ Exn.message) pure

    lift $
      H.modify
        $ (JTS._isEnteringPageSize .~ false)
        ∘ (JTS._result ?~
             { json: JSON.fromArray items
             , page: p.page
             , pageSize: p.pageSize
             })

    pure inputPort

  Just Port.Blocked → do
    lift $ resetState
    pure Nothing

  _ → throwError "Expected a TaggedResource input"

-- | Resets the state while preserving settings like page size.
resetState ∷ H.ComponentDSL JTS.State QueryP Slam Unit
resetState = H.modify (JTS._result .~ Nothing)

-- | Evaluates jtable-specific card queries.
evalJTable ∷ Natural Query (H.ComponentDSL JTS.State QueryP Slam)
evalJTable (StepPage step next) =
  H.modify (JTS.stepPage step) $> next
evalJTable (ChangePageSize pageSize next) =
  maybe (pure unit) (H.modify ∘ JTS.resizePage) (Int.fromString pageSize) $> next
evalJTable (StartEnterCustomPageSize next) =
  H.modify (JTS._isEnteringPageSize .~ true) $> next
evalJTable (SetCustomPageSize size next) =
  H.modify (JTS.setPageSize size) $> next
evalJTable (SetCustomPage page next) =
  H.modify (JTS.setPage page) $> next
evalJTable (UpdateLevelOfDetail next) = do
  Debug.Trace.traceAnyA "update lod"
  updateLevelOfDetail $> next
