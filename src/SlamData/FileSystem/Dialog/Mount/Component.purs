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

module SlamData.FileSystem.Dialog.Mount.Component
  ( component
  , module SlamData.FileSystem.Dialog.Mount.Component.Query
  , module MCS
  ) where

import SlamData.Prelude

import Data.Argonaut (jsonParser, decodeJson, (.?))
import Data.Lens (set, (.~), (?~))

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.FileSystem.Dialog.Component.Message (Message(..))
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery as SQ
import SlamData.FileSystem.Dialog.Mount.Component.ChildSlot as CS
import SlamData.FileSystem.Dialog.Mount.Component.Query (Query(..))
import SlamData.FileSystem.Dialog.Mount.Component.State as MCS
import SlamData.FileSystem.Dialog.Mount.Couchbase.Component as Couchbase
import SlamData.FileSystem.Dialog.Mount.MarkLogic.Component as MarkLogic
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component as MongoDB
import SlamData.FileSystem.Dialog.Mount.Scheme as MS
import SlamData.FileSystem.Dialog.Mount.SparkHDFS.Component as Spark
import SlamData.FileSystem.Dialog.Mount.SQL2.Component as SQL2
import SlamData.GlobalError as GE
import SlamData.Monad (Slam)
import SlamData.Quasar.FS as Api
import SlamData.Render.CSS as RC

import Utils.DOM as DOM

type DSL = H.ParentDSL MCS.State Query CS.ChildQuery CS.ChildSlot Message Slam
type HTML = H.ParentHTML Query CS.ChildQuery CS.ChildSlot Slam

component ∷ H.Component HH.HTML Query MCS.Input Message Slam
component =
  H.parentComponent
    { initialState: MCS.initialState
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ MCS.State → HTML
render state@{ new } =
  modalDialog
    [ modalHeader "Mount"
    , modalBody $
        HH.form
          [ HE.onSubmit $ HE.input PreventDefault
          , HP.class_ RC.dialogMount
          ]
          $ (guard new $> fldName state)
          <> (guard new $> selScheme state)
          <> maybe [] (pure ∘ settings) state.settings
          <> maybe [] (pure ∘ errorMessage) state.message
    , modalFooter
        [ progressSpinner state
        , btnCancel
        , btnMount state
        ]
    ]
  where
  settings ∷ MCS.MountSettings → HTML
  settings ss = case ss of
    MCS.MongoDB initialState →
      HH.slot' CS.cpMongoDB unit MongoDB.comp unit (HE.input_ Validate)
    MCS.SQL2 initialState →
      HH.slot' CS.cpSQL unit SQL2.comp unit (HE.input_ Validate)
    MCS.Couchbase initialState →
      HH.slot' CS.cpCouchbase unit Couchbase.comp unit (HE.input_ Validate)
    MCS.MarkLogic initialState →
      HH.slot' CS.cpMarkLogic unit MarkLogic.comp unit (HE.input_ Validate)
    MCS.SparkHDFS initialState →
      HH.slot' CS.cpSpark unit Spark.comp unit (HE.input_ Validate)

fldName ∷ MCS.State → HTML
fldName state =
  HH.div
    [ HP.classes [B.formGroup, RC.mountName] ]
    [ HH.label_
        [ HH.span_ [ HH.text "Name" ]
        , HH.input
            [ HP.class_ B.formControl
            , HE.onValueInput $ HE.input (ModifyState ∘ set MCS._name)
            , HP.value (state.name)
            ]
        ]
    ]

selScheme ∷ MCS.State → HTML
selScheme state =
  HH.div
    [ HP.class_ B.formGroup ]
    [ HH.label_
        [ HH.span_ [ HH.text "Mount type" ]
        , HH.select
            [ HP.class_ B.formControl
            , HE.onValueChange (HE.input SelectScheme ∘ MS.schemeFromString)
            ]
            $ [ HH.option_ [] ] <> schemeOptions
        ]
    ]
  where
  schemeOptions = map (\s → HH.option_ [ HH.text (MS.schemeToString s) ]) MS.schemes

errorMessage ∷ String → HTML
errorMessage msg =
  HH.div
    [ HP.classes [ B.alert, B.alertDanger ] ]
    [ HH.text msg ]

btnCancel ∷ HTML
btnCancel =
  HH.button
    [ HP.classes [B.btn]
    , HE.onClick (HE.input_ RaiseDismiss)
    ]
    [ HH.text "Cancel" ]

btnMount ∷ MCS.State → HTML
btnMount state@{ new, saving } =
  HH.button
    [ HP.classes [B.btn, B.btnPrimary]
    , HP.enabled (not saving && MCS.canSave state)
    , HE.onClick (HE.input_ NotifySave)
    ]
    [ HH.text text ]
  where
  text = if new then "Mount" else "Save changes"

progressSpinner ∷ MCS.State → HTML
progressSpinner { saving } =
  HH.img [ HP.src "img/spin.gif", HP.class_ (RC.mountProgressSpinner saving) ]

eval ∷ Query ~> DSL
eval (ModifyState f next) = H.modify f *> validateInput $> next
eval (SelectScheme newScheme next) = do
  currentScheme ← map MCS.scheme <$> H.gets _.settings
  when (currentScheme /= newScheme) do
    H.modify (MCS._settings .~ map MCS.initialSettings newScheme)
    validateInput
  pure next
eval (RaiseDismiss next) = do
  H.raise Dismiss
  pure next
eval (NotifySave next) = do
  H.raise MountSave
  pure next
eval (Save k) = do
  { parent, name, new } ← H.get
  H.modify (MCS._saving .~ true)
  newName ←
    if new then Api.getNewName parent name else pure (pure name)
  case newName of
    Left err → do
      handleQError err
      pure $ k Nothing
    Right newName' → do
      result ← querySettings (H.request (SQ.Submit parent newName'))
      mount ← case result of
        Just (Right m) → pure (Just m)
        Just (Left err) → do
          handleQError err
          pure Nothing
        Nothing → pure Nothing
      H.modify (MCS._saving .~ false)
      pure $ k mount
eval (PreventDefault ev next) = H.liftEff (DOM.preventDefault ev) $> next
eval (Validate next) = validateInput $> next

handleQError ∷ Api.QError → DSL Unit
handleQError err =
  case GE.fromQError err of
    Left msg → H.modify (MCS._message ?~ formatError msg)
    Right ge → GE.raiseGlobalError ge

validateInput ∷ DSL Unit
validateInput = do
  state ← H.get
  message ← runExceptT do
    liftMaybe (MCS.validate state)
    liftMaybe ∘ join =<< lift (querySettings (H.request SQ.Validate))
  H.modify (MCS._message .~ either Just (const Nothing) message)
  where
  liftMaybe ∷ Maybe String → ExceptT String DSL Unit
  liftMaybe = maybe (pure unit) throwError

formatError ∷ String → String
formatError err =
  "There was a problem saving the mount: " <> extract err
  where
  extract msg =
    either (const msg) id (jsonParser msg >>= decodeJson >>= (_ .? "error"))

querySettings ∷ forall a. (forall s. SQ.SettingsQuery s a) → DSL (Maybe a)
querySettings q = (map MCS.scheme <$> H.gets _.settings) >>= \s →
  case s of
    Just MS.MongoDB → H.query' CS.cpMongoDB unit q
    Just MS.SQL2 → H.query' CS.cpSQL unit q
    Just MS.Couchbase → H.query' CS.cpCouchbase unit q
    Just MS.MarkLogic → H.query' CS.cpMarkLogic unit q
    Just MS.SparkHDFS → H.query' CS.cpSpark unit q
    _ → pure Nothing
