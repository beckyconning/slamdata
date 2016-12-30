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

module SlamData.Workspace.Eval.Persistence where

import SlamData.Prelude

import Control.Comonad.Cofree as Cofree
import Control.Monad.Aff.AVar (AVar, modifyVar, killVar, peekVar, putVar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Fork (class MonadFork, fork)
import Control.Monad.Throw (class MonadThrow, throw)

import Data.Array as Array
import Data.List as List
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Path.Pathy as Pathy
import Data.Path.Pathy ((</>))
import Data.Rational ((%))
import Data.Set (Set)
import Data.Set as Set

import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Data as Quasar
import SlamData.Quasar.Error as QE
import SlamData.Wiring (Wiring)
import SlamData.Wiring as Wiring
import SlamData.Wiring.Cache as Cache
import SlamData.Workspace.AccessType (isEditable)
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Draftboard.Layout as Layout
import SlamData.Workspace.Card.Draftboard.Orientation as Orn
import SlamData.Workspace.Card.Draftboard.Pane as Pane
import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port (Port)
import SlamData.Workspace.Deck.DeckId as DID
import SlamData.Workspace.Deck.Model as DM
import SlamData.Workspace.Eval as Eval
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck
import SlamData.Workspace.Eval.Graph (unfoldGraph, EvalGraph)
import SlamData.Workspace.Eval.Traverse (TraverseCard(..), TraverseDeck(..), unfoldModelTree)
import SlamData.Workspace.Model as WM
import SlamData.Workspace.Legacy (isLegacy, loadCompatWorkspace, pruneLegacyData)

import Utils.Aff (laterVar)

defaultSaveDebounce ∷ Int
defaultSaveDebounce = 500

defaultEvalDebounce ∷ Int
defaultEvalDebounce = 500

type Persist f m a =
  ( MonadAff SlamDataEffects m
  , MonadAsk Wiring m
  , MonadFork Exn.Error m
  , MonadThrow Exn.Error m
  , Parallel f m
  , QuasarDSL m
  ) ⇒ a

type PersistEnv m a =
  ( MonadAff SlamDataEffects m
  , MonadAsk Wiring m
  ) ⇒ a

type ForkAff m a =
  ( MonadAff SlamDataEffects m
  , MonadFork Exn.Error m
  ) ⇒ a

loadWorkspace ∷ ∀ f m. Persist f m (m (Either QE.QError Deck.Id))
loadWorkspace = runExceptT do
  { path, eval, accessType } ← Wiring.expose
  stat × ws ← ExceptT $ loadCompatWorkspace path
  liftAff $ putVar eval.root ws.rootId
  graph ← unwrapOrThrow (QE.msgToQError "Cannot build graph") $
    unfoldModelTree ws.decks ws.cards ws.rootId
  lift $ populateGraph mempty mempty Nothing graph
  when (isLegacy stat && isEditable accessType) do
    ExceptT saveWorkspace
    void $ lift $ pruneLegacyData path -- Not imperative that this succeeds
  pure ws.rootId

saveWorkspace ∷ ∀ f m. Persist f m (m (Either QE.QError Unit))
saveWorkspace = runExceptT do
  { path, eval } ← Wiring.expose
  decks ← map _.model <$> Cache.snapshot eval.decks
  cards ← map _.model <$> Cache.snapshot eval.cards
  rootId ← liftAff $ peekVar eval.root
  let
    json = WM.encode { rootId, decks, cards }
    file = path </> Pathy.file "index"
  ExceptT $ Quasar.save file json

putDeck ∷ ∀ m. PersistEnv m (Deck.Id → Deck.Model → m Unit)
putDeck deckId deck = do
  { eval } ← Wiring.expose
  Cache.alter deckId (pure ∘ map _ { model = deck }) eval.decks

getDeck ∷ ∀ m. PersistEnv m (Deck.Id → m (Maybe Deck.Cell))
getDeck deckId = do
  { eval } ← Wiring.expose
  Cache.get deckId eval.decks

putCard ∷ ∀ m. PersistEnv m (Card.Id → Card.Model → m Unit)
putCard cardId card = do
  { eval } ← Wiring.expose
  Cache.alter cardId (pure ∘ map _ { model = card }) eval.cards

getCard ∷ ∀ m. PersistEnv m (Card.Id → m (Maybe Card.Cell))
getCard cardId = do
  { eval } ← Wiring.expose
  Cache.get cardId eval.cards

getCards ∷ ∀ m. PersistEnv m (Array Card.Id → m (Maybe (Array Card.Cell)))
getCards = map sequence ∘ traverse getCard

getEvaluatedCards ∷ ∀ m. PersistEnv m (Array Card.Id → m (Maybe (Array Card.Cell × Port)))
getEvaluatedCards = map (flip bind (Array.foldM go (mempty × Port.Initial))) ∘ getCards
  where
    go acc@(cs × Card.CardError _) _ = pure acc
    go acc@(cs × _) card@{ tick: Just _, output: Just out } =
      pure (Array.snoc cs card × out)
    go _ _ = Nothing

publishCardChange ∷ ∀ f m. Persist f m (Card.DisplayCoord → Card.Model → m Unit)
publishCardChange source@(_ × cardId) model = do
  putCard cardId model
  mbGraph ← snapshotGraph cardId
  for_ mbGraph \graph → do
    queueEval' defaultEvalDebounce source graph
    queueSaveDefault
    let card = (Cofree.head graph).card
    liftAff $ Bus.write (Card.ModelChange source model) card.bus
    for_ card.decks \deckId →
      getDeck deckId >>= traverse_ \deck →
        liftAff $ Bus.write (Deck.CardChange cardId) deck.bus

populateGraph
  ∷ ∀ f m
  . Persist f m
  ( Map Deck.Id Deck.Cell
  → Map Card.Id Card.Cell
  → Maybe Card.Id
  → TraverseDeck Deck.Model Card.Model
  → m Unit )
populateGraph oldDecks oldCards rootParent root = do
  { eval } ← Wiring.expose
  goDeck eval rootParent root
  where
    goDeck eval parent (TraverseDeck { deckId, deck, cards }) = do
      cell ← Cache.get deckId eval.decks >>= case _, Map.lookup deckId oldDecks of
        Just cell, _ → pure cell
        _, Just cell → pure cell { model = deck }
        _, _ → makeDeckCell deck
      cardCells ←
        List.foldM (goCard eval deckId) mempty cards >>= case _ of
          (lastId × last) : tail -> do
            let last' = last { next = Set.insert (Left deckId) last.next }
            pure ((lastId × last') : tail)
          _ → do
            pure mempty
      Cache.put deckId (cell { parent = parent }) eval.decks
      for_ cardCells (flip (uncurry Cache.put) eval.cards)
      for_ cards \(TraverseCard { cardId, decks }) →
        for_ decks (goDeck eval (Just cardId))

    goCard eval deckId prevCells (TraverseCard { cardId, card }) = do
      cell ← Cache.get cardId eval.cards >>= case _, Map.lookup cardId oldCards of
        Just cell, _ → pure cell
        _, Just cell → pure $ cell { decks = Set.empty, next = Set.empty } ∷ Card.Cell
        _, _ → makeCardCell Nothing Set.empty card
      let cell' = cell { decks = Set.insert deckId cell.decks }
      case prevCells of
        (prevId × prev) : tail → do
          let prev' = prev { next = Set.insert (Right cardId) prev.next }
          pure ((cardId × cell') : (prevId × prev') : tail)
        _ → do
          pure (List.singleton (cardId × cell'))

rebuildGraph ∷ ∀ f m . Persist f m (m Unit)
rebuildGraph = do
  { eval } ← Wiring.expose
  decks ← Cache.snapshot eval.decks
  cards ← Cache.snapshot eval.cards
  rootId ← liftAff $ peekVar eval.root
  graph ← unwrapOrExn "Cannot rebuild graph" $
    unfoldModelTree (_.model <$> decks) (_.model <$> cards) rootId
  Cache.restore mempty eval.decks
  Cache.restore mempty eval.cards
  populateGraph decks cards Nothing graph

snapshotGraph ∷ ∀ f m. Persist f m (Card.Id → m (Maybe EvalGraph))
snapshotGraph cardId = do
  { eval } ← Wiring.expose
  unfoldGraph
    <$> Cache.snapshot eval.cards
    <*> Cache.snapshot eval.decks
    <*> pure cardId

queueSave ∷ ∀ f m. Persist f m (Int → m Unit)
queueSave ms = do
  { eval, path, accessType } ← Wiring.expose
  when (isEditable accessType) do
    debounce ms path { avar: _ } eval.pendingSaves (pure unit) do
      void saveWorkspace

queueSaveImmediate ∷ ∀ f m. Persist f m (m Unit)
queueSaveImmediate = queueSave 0

queueSaveDefault ∷ ∀ f m. Persist f m (m Unit)
queueSaveDefault = queueSave defaultSaveDebounce

queueEval' ∷ ∀ f m. Persist f m (Int → Card.DisplayCoord → EvalGraph → m Unit)
queueEval' ms source@(_ × cardId) graph = do
  { eval } ← Wiring.expose
  let pending = { source, graph, avar: _ }
  debounce ms cardId pending eval.pendingEvals
    (Eval.notifyDecks (Deck.Pending cardId) graph)
    (Eval.evalGraph source graph)

queueEval ∷ ∀ f m. Persist f m (Int → Card.DisplayCoord → m Unit)
queueEval ms source@(_ × cardId) =
  traverse_ (queueEval' ms source) =<< snapshotGraph cardId

queueEvalImmediate ∷ ∀ f m. Persist f m (Card.DisplayCoord → m Unit)
queueEvalImmediate = queueEval 0

queueEvalDefault ∷ ∀ f m. Persist f m (Card.DisplayCoord → m Unit)
queueEvalDefault = queueEval defaultEvalDebounce

queueEvalForDeck ∷ ∀ f m. Persist f m (Int → Deck.Id → m Unit)
queueEvalForDeck ms deckId =
  getDeck deckId >>= traverse_ \cell →
    for_ (Array.head cell.model.cards) (queueEval ms ∘ Card.toAll)

freshWorkspace ∷ ∀ f m. Persist f m (Array CM.AnyCardModel → m (Deck.Id × Deck.Cell))
freshWorkspace anyCards = do
  { eval } ← Wiring.expose
  genCards ← traverse genCard anyCards
  deckId ← liftAff DID.make
  bus ← liftAff Bus.make
  let
    cards = Map.fromFoldable genCards
    deck = Deck.emptyDeck { cards = fst <$> genCards }
    cell = { bus, parent: Nothing, model: deck }
  graph ← unwrapOrExn "Cannot create workspace" $
    unfoldModelTree (Map.singleton deckId deck) cards deckId
  Cache.put deckId cell eval.decks
  liftAff $ putVar eval.root deckId
  populateGraph Map.empty Map.empty Nothing graph
  pure (deckId × cell)
  where
    genCard model = do
      cardId ← liftAff CID.make
      pure (cardId × model)

freshDeck ∷ ∀ m. PersistEnv m (Deck.Model → m (Deck.Id × Deck.Cell))
freshDeck model = do
  { eval } ← Wiring.expose
  deckId ← liftAff DID.make
  cell ← makeDeckCell model
  Cache.put deckId cell eval.decks
  pure (deckId × cell)

freshCard ∷ ∀ f m. Persist f m (Maybe Port → Set (Either Deck.Id Card.Id) → Card.Model → m (Card.Id × Card.Cell))
freshCard input next model = do
  { eval } ← Wiring.expose
  cardId ← liftAff CID.make
  cell ← makeCardCell input next model
  Cache.put cardId cell eval.cards
  pure (cardId × cell)

deleteDeck ∷ ∀ f m. Persist f m (Deck.Id → m (Maybe Card.Id))
deleteDeck deckId = do
  { eval, path } ← Wiring.expose
  cell ← unwrapOrExn "Deck not found" =<< getDeck deckId
  Cache.remove deckId eval.decks
  case cell.parent of
    Nothing → do
      rootId ← liftAff $ peekVar eval.root
      when (rootId ≡ deckId) $ void do
        Quasar.delete (Left path)
    Just oldParentId → do
      oldParentModel ← updatePointer deckId Nothing oldParentId
      putCard oldParentId oldParentModel
      rebuildGraph
      publishCardChange (Card.toAll oldParentId) oldParentModel
      queueSaveDefault
  pure cell.parent

wrapDeck ∷ ∀ f m. Persist f m (Deck.Id → m Deck.Id)
wrapDeck deckId = do
  cell ← unwrapOrExn "Deck not found" =<< getDeck deckId
  parentCardId × _ ← freshCard (Just Port.Initial) Set.empty (CM.singletonDraftboard deckId)
  parentDeckId × _ ← freshDeck DM.emptyDeck { cards = pure parentCardId }
  updateRootOrParent deckId parentDeckId cell.parent
  queueSaveDefault
  pure parentDeckId

wrapAndMirrorDeck ∷ ∀ f m. Persist f m (Deck.Id → m Deck.Id)
wrapAndMirrorDeck deckId = do
  cell ← unwrapOrExn "Deck not found" =<< getDeck deckId
  mirrorDeckId × _ ← freshDeck DM.emptyDeck { cards = cell.model.cards }
  parentCardId × _ ←
    freshCard (Just Port.Initial) Set.empty $
      CM.splitDraftboard Orn.Vertical (List.fromFoldable [ deckId, mirrorDeckId ])
  parentDeckId × _ ← freshDeck DM.emptyDeck { cards = pure parentCardId }
  cloneActiveStateTo mirrorDeckId deckId
  updateRootOrParent deckId parentDeckId cell.parent
  queueSaveDefault
  pure parentDeckId

mirrorDeck ∷ ∀ f m. Persist f m (Deck.Id → Card.Id → m Deck.Id)
mirrorDeck deckId parentId = do
  cell ← unwrapOrExn "Deck not found" =<< getDeck deckId
  card ← unwrapOrExn "Card not found" =<< getCard parentId
  mirrorDeckId × _ ← freshDeck DM.emptyDeck { cards = cell.model.cards }
  parentModel ← unwrapOrExn "Cannot mirror deck" $ CM.mirrorInDraftboard deckId mirrorDeckId card.model
  cloneActiveStateTo mirrorDeckId deckId
  putCard parentId parentModel
  rebuildGraph
  publishCardChange (Card.toAll parentId) parentModel
  queueSaveDefault
  pure mirrorDeckId

unwrapDeck ∷ ∀ f m. Persist f m (Deck.Id → m Deck.Id)
unwrapDeck deckId = do
  { eval } ← Wiring.expose
  cell ← unwrapOrExn "Deck not found" =<< getDeck deckId
  cards ← unwrapOrExn "Cards not found" =<< getCards cell.model.cards
  childId ← unwrapOrExn "Cannot unwrap deck" $ immediateChild (_.model <$> cards)
  child ← unwrapOrExn "Child not found" =<< getDeck childId
  Cache.remove deckId eval.decks
  updateRootOrParent deckId childId cell.parent
  queueSaveDefault
  pure childId
  where
    immediateChild ∷ Array Card.Model → Maybe Deck.Id
    immediateChild = case _ of
      [ model ] →
        case CM.childDeckIds model of
          childId : Nil → Just childId
          _ → Nothing
      _ → Nothing

collapseDeck ∷ ∀ f m. Persist f m (Deck.Id → Card.Id → m Unit)
collapseDeck deckId cardId = do
  { eval } ← Wiring.expose
  cell ← unwrapOrExn "Deck not found" =<< getDeck deckId
  cards ← unwrapOrExn "Cards not found" =<< getCards cell.model.cards
  parent ← unwrapOrExn "Parent not found" =<< getCard cardId
  let
    parentModel = parent.model
    cardModels = _.model <$> cards
  parentModel' ← unwrapOrExn "Cannot collapse deck" $ collapse parentModel cardModels
  putCard cardId parentModel'
  rebuildGraph
  publishCardChange (Card.toAll cardId) parentModel'
  queueSaveDefault
  where
    collapse ∷ Card.Model → Array Card.Model → Maybe Card.Model
    collapse parent child = case parent, child of
      CM.Draftboard { layout }, [ cm@CM.Draftboard { layout: subLayout } ] → do
        cursor ← Pane.getCursorFor (Just deckId) layout
        layout' ← Pane.modifyAt (const subLayout) cursor layout
        pure $ CM.Draftboard { layout: layout' }
      _, _ → Nothing

wrapAndGroupDeck ∷ ∀ f m. Persist f m (Orn.Orientation → Layout.SplitBias → Deck.Id → Deck.Id → m Unit)
wrapAndGroupDeck orn bias deckId siblingId = do
  cell ← unwrapOrExn "Deck not found" =<< getDeck deckId
  sibling ← unwrapOrExn "Sibling not found" =<< getDeck siblingId
  oldParentId ← unwrapOrExn "Parent not found" cell.parent
  oldParent ← unwrapOrExn "Parent not found" =<< getCard oldParentId
  case oldParent.model of
    CM.Draftboard { layout } → do
      let
        splits = case bias of
          Layout.SideA → [ deckId, siblingId ]
          Layout.SideB → [ siblingId, deckId ]
      parentCardId × _ ← freshCard (Just Port.Initial) Set.empty $ CM.splitDraftboard orn (List.fromFoldable splits)
      parentDeckId × _ ← freshDeck DM.emptyDeck { cards = pure parentCardId }
      let
        layout' = layout <#> case _ of
          Just did | did ≡ deckId → Nothing
          Just did | did ≡ siblingId → Just parentDeckId
          a → a
        oldParent' =
          CM.Draftboard { layout: layout' }
      putCard oldParentId oldParent'
      rebuildGraph
      publishCardChange (Card.toAll oldParentId) oldParent'
      queueSaveDefault
    _ → do
      throw (Exn.error "Cannot group deck")

groupDeck ∷ ∀ f m. Persist f m (Orn.Orientation → Layout.SplitBias → Deck.Id → Deck.Id → Card.Id → m Unit)
groupDeck orn bias deckId siblingId newParentId = do
  cell ← unwrapOrExn "Deck not found" =<< getDeck deckId
  oldParentId ← unwrapOrExn "Parent not found" cell.parent
  oldParent ← unwrapOrExn "Parent not found" =<< getCard oldParentId
  newParent ← unwrapOrExn "Destination not found" =<< getCard newParentId
  case oldParent.model, newParent.model of
    CM.Draftboard { layout }, CM.Draftboard { layout: inner } → do
      let
        inner' =
          Layout.insertRootSplit (Pane.Cell (Just deckId)) orn (1%2) bias layout
        layout' = layout <#> case _ of
          Just did | did ≡ deckId → Nothing
          a → a
        child' = CM.Draftboard { layout: inner' }
        parent' = CM.Draftboard { layout: layout' }
      publishCardChange (Card.toAll newParentId) child'
      publishCardChange (Card.toAll oldParentId) parent'
      queueSaveDefault
    _, _ →
      wrapAndGroupDeck orn bias deckId siblingId

renameDeck ∷ ∀ f m. Persist f m (Deck.Id → String → m Unit)
renameDeck deckId name = do
  cell ← unwrapOrExn "Deck not found" =<< getDeck deckId
  putDeck deckId (cell.model { name = name })
  liftAff $ Bus.write (Deck.NameChange name) cell.bus
  queueSaveDefault
  pure unit

addCard ∷ ∀ f m. Persist f m (Deck.Id → CT.CardType → m Card.Id)
addCard deckId cty = do
  { eval } ← Wiring.expose
  deck ← unwrapOrExn "Deck not found" =<< getDeck deckId
  input ← fromMaybe Port.Initial <$> runMaybeT do
    last ← MaybeT $ pure $ Array.last deck.model.cards
    card ← MaybeT $ getCard last
    MaybeT $ pure card.output
  cardId × _ ← freshCard (Just input) Set.empty $ Card.cardModelOfType input cty
  putDeck deckId deck.model { cards = Array.snoc deck.model.cards cardId }
  rebuildGraph
  liftAff $ Bus.write (Deck.CardChange cardId) deck.bus
  queueSaveDefault
  queueEvalImmediate (Card.toAll cardId)
  pure cardId

removeCard ∷ ∀ f m. Persist f m (Deck.Id → Card.Id → m Unit)
removeCard deckId cardId = do
  { eval } ← Wiring.expose
  deck ← unwrapOrExn "Deck not found" =<< getDeck deckId
  let
    cardIds = Array.span (not ∘ eq cardId) deck.model.cards
    deck' = deck.model { cards = cardIds.init }
  output ← runMaybeT do
    last ← MaybeT $ pure $ Array.last cardIds.init
    card ← MaybeT $ getCard last
    let next' = Set.insert (Left deckId) $ Set.delete (Right cardId) card.next
    lift $ Cache.put last (card { next = next' }) eval.cards
    MaybeT $ pure card.output
  putDeck deckId deck'
  rebuildGraph
  queueSaveDefault
  liftAff $ Bus.write (Deck.Complete cardIds.init (fromMaybe Port.Initial output)) deck.bus

updateRootOrParent ∷ ∀ f m. Persist f m (Deck.Id → Deck.Id → Maybe Card.Id → m Unit)
updateRootOrParent oldId newId = case _ of
  Nothing → do
    updateRoot newId
    rebuildGraph
  Just oldParentId → do
    oldParentModel ← updatePointer oldId (Just newId) oldParentId
    putCard oldParentId oldParentModel
    rebuildGraph
    publishCardChange (Card.toAll oldParentId) oldParentModel

updatePointer ∷ ∀ f m. Persist f m (Deck.Id → Maybe Deck.Id → Card.Id → m Card.Model)
updatePointer oldId newId parentId = do
  cell ← unwrapOrExn "Card not found" =<< getCard parentId
  pure $ CM.updatePointer oldId newId cell.model

updateRoot ∷ ∀ f m. Persist f m (Deck.Id → m Unit)
updateRoot newId = do
  { eval } ← Wiring.expose
  liftAff $ modifyVar (const newId) eval.root

cloneActiveStateTo ∷ ∀ f m. Persist f m (Deck.Id → Deck.Id → m Unit)
cloneActiveStateTo to from = do
  { cache } ← Wiring.expose
  activeState ← Cache.get from cache.activeState
  for_ activeState \as → Cache.put to as cache.activeState

makeDeckCell
  ∷ ∀ m
  . MonadAff SlamDataEffects m
  ⇒ Deck.Model
  → m Deck.Cell
makeDeckCell model = do
  bus ← liftAff Bus.make
  pure { bus, model, parent: Nothing }

makeCardCell
  ∷ ∀ m
  . MonadAff SlamDataEffects m
  ⇒ Maybe Port
  → Set (Either Deck.Id Card.Id)
  → Card.AnyCardModel
  → m Card.Cell
makeCardCell input next model = do
  bus ← liftAff Bus.make
  pure $
    { bus
    , next
    , decks: Set.empty
    , model
    , input
    , output: Nothing
    , state: Nothing
    , sources: Set.empty
    , tick: Nothing
    } ∷ Card.Cell

debounce
  ∷ ∀ k m r
  . ForkAff m
  ( Ord k
  ⇒ Int
  → k
  → (AVar Unit → { avar ∷ AVar Unit | r })
  → Cache.Cache k { avar ∷ AVar Unit | r }
  → m Unit
  → m Unit
  → m Unit )
debounce ms key make cache init run = do
  avar ← laterVar ms $ void $ run *> Cache.remove key cache
  Cache.alter key (alterFn (make avar)) cache
  where
    alterFn a b = do
      case b of
        Just { avar } → liftAff $ killVar avar (Exn.error "debounce")
        Nothing → void $ fork init
      pure (Just a)

unwrapOrExn ∷ ∀ m a. MonadThrow Exn.Error m ⇒ String → Maybe a → m a
unwrapOrExn = unwrapOrThrow ∘ Exn.error

unwrapOrThrow ∷ ∀ m e a. MonadThrow e m ⇒ e → Maybe a → m a
unwrapOrThrow err = maybe (throw err) pure
