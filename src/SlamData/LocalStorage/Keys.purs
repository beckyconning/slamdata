module SlamData.LocalStorage.Keys where

import SlamData.Prelude

import Data.Map (Map)
import Quasar.Advanced.Types as QAT
import SlamData.LocalStorage.Class as LS
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Deck.DeckId as DID
import SlamData.Workspace.Eval.Card (AnyCardModel)
import SlamData.Workspace.Eval.Deck as Deck

--- Consumer card persistence

cardsLocalStorageKey ∷ Deck.Id → LS.Key (Map CID.CardId AnyCardModel)
cardsLocalStorageKey =
  LS.Key ∘ ("sd-cards-" ⊕ _) ∘ DID.toString

--- Hint dismissals

dismissedFocusDeckHintKey ∷ LS.Key Boolean
dismissedFocusDeckHintKey = LS.Key "dismissedFocusDeckHint"

dismissedFocusDeckFrameHintKey ∷ LS.Key Boolean
dismissedFocusDeckFrameHintKey = LS.Key "dismissedFocusDeckFrameHint"

dismissedAccessNextActionCardHintKey ∷ LS.Key Boolean
dismissedAccessNextActionCardHintKey = LS.Key "dismissedAccessNextActionCardHint"

dismissedAddCardHintKey ∷ LS.Key Boolean
dismissedAddCardHintKey = LS.Key "dismissedAddCardHint"

dismissedMountHintKey ∷ LS.Key Boolean
dismissedMountHintKey = LS.Key "dismissed-mount-guide"

-- Video dismissals

dismissedIntroVideoKey ∷ LS.Key Boolean
dismissedIntroVideoKey = LS.Key "dismissed-intro-video"

-- Guide dismissals

dismissedCardGuideKey ∷ LS.Key Boolean
dismissedCardGuideKey = LS.Key "dismissedCardGuide"

dismissedFlipGuideKey ∷ LS.Key Boolean
dismissedFlipGuideKey = LS.Key "dismissedFlipGuide"

-- Authentication

idTokenLocalStorageKey ∷ String -> LS.Key (Either String String)
idTokenLocalStorageKey = LS.Key ∘ ("sd-auth-id-token-" ⊕ _)

keyStringLocalStorageKey ∷ String -> LS.Key String
keyStringLocalStorageKey = LS.Key ∘ ("sd-auth-csrf-" ⊕ _)

nonceLocalStorageKey ∷ String -> LS.Key String
nonceLocalStorageKey = LS.Key ∘ ("sd-auth-replay-" ⊕ _)

providerLocalStorageKey ∷ String -> LS.Key QAT.Provider
providerLocalStorageKey = LS.Key ∘ ("sd-auth-provider-" ⊕ _)

-- Autocomplete

autoCompletePathsKey :: LS.Key (Array String)
autoCompletePathsKey = LS.Key "sd-autocomplete-paths"

