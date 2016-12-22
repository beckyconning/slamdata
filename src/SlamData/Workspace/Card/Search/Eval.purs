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

module SlamData.Workspace.Card.Search.Eval
  ( evalSearch
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Throw (class MonadThrow)
import Control.Monad.Writer.Class (class MonadTell)

import Data.Path.Pathy as Path
import Data.StrMap as SM

import Quasar.Types (FilePath)

import SlamData.Effects (SlamDataEffects)
import SlamData.GlobalError as GE
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL)
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Eval.Common (validateResources)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Search.Interpret as Search

import Text.SlamSearch as SS

evalSearch
  ∷ ∀ m
  . ( MonadAff SlamDataEffects m
    , MonadAsk CEM.CardEnv m
    , MonadThrow CEM.CardError m
    , MonadTell CEM.CardLog m
    , QuasarDSL m
    , ParQuasarDSL m
    )
  ⇒ String
  → FilePath
  → m Port.TaggedResourcePort
evalSearch queryText resource = do
  query ← case SS.mkQuery queryText of
    Left _ → CEM.throw "Incorrect query string"
    Right q → pure q

  fields ← CEM.liftQ do
    QFS.messageIfFileNotFound
      resource
      ("Input resource " ⊕ Path.printPath resource ⊕ " doesn't exist")
    QQ.fields resource

  outputResource ← CEM.temporaryOutputResource

  let
    template = Search.queryToSQL fields query
    sql = QQ.templated resource template

  compileResult ← QQ.compile (Right resource) sql SM.empty
  case compileResult of
    Left err →
      case GE.fromQError err of
        Left msg → CEM.throw $ "Error compiling query: " ⊕ msg
        Right _ → CEM.throw $ "Error compiling query: " ⊕ QE.printQError err
    Right { inputs } → do
      validateResources inputs
      CEM.addSources inputs

  CEM.liftQ do
    QQ.viewQuery (Right resource) outputResource template SM.empty
    QFS.messageIfFileNotFound
      outputResource
      "Error making search temporary resource"

  pure { resource: outputResource, tag: pure sql, varMap: Nothing }
