{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Setups.Chart.Common where

import Data.Argonaut as J
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Setups.Dimension as D
import SqlSquare as Sql

projectJCursor ∷ ∀ a. D.Dimension a J.JCursor → Sql.Projection Sql.Sql
projectJCursor (D.Dimension _ cat) = Sql.projection case cat of
  D.Static str → Sql.string str
  D.Projection _ pr → QQ.jcursorToSql pr

projectNull ∷ Sql.Projection Sql.Sql
projectNull = Sql.projection Sql.null
