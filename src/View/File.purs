module View.File (I(), view) where

import Control.Alt ((<|>))
import Control.Alternative (Alternative)
import Control.Apply ((*>))
import Control.Functor (($>))
import Control.Inject1 (inj)
import Control.Monad.Aff
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)
import Controller.File
import Data.Array ((..), length, zipWith)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Input.File.Item (ItemInput(..))
import Model.Breadcrumb
import Model.File
import Model.Item
import Model.Path
import Model.Resource
import Model.Sort
import Utils.Halide (targetLink', readonly)
import View.File.Modal
import View.File.Breadcrumb
import EffectTypes
import qualified Data.StrMap as SM
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import qualified Halogen.Themes.Bootstrap3 as B
import qualified Utils as U
import qualified View.Css as Vc

type I e = E.Event (FileAppEff e) Input

homeHash :: String
homeHash = "#?sort=asc&q=path%3A%2F&salt="

glyph :: forall p i. A.ClassName -> H.HTML p i
glyph g = H.i [ A.classes [B.glyphicon, g] ] []

icon :: forall p i. H.HTML p i
icon = H.div [ A.classes [ B.colXs1, Vc.navIcon ] ]
             [ H.a [ A.href homeHash
                   , A.classes [B.navbarBrand, Vc.logo]
                   ]
                   [ glyph B.glyphiconFolderOpen ]
             ]

logo :: forall p i. H.HTML p i
logo = H.div [ A.classes [ B.colXs3, Vc.navLogo ] ]
             [ H.a [ A.href Config.slamDataHome
                   , A.classes [B.navbarBrand, Vc.logo]
                   ]
                   [ H.img [A.src "img/logo.svg"]
                           []
                   ]
             ]

search :: forall p e. (Request -> I e) -> State -> H.HTML p (I e)
search handler state =
  H.div [ A.classes [B.colXs12, B.colSm8, Vc.search] ]
        [ H.form [ A.class_ B.navbarForm
                 , E.onSubmit (\_ -> pure $ handler $ SearchSubmit state.search state.path)
                 ]
                 [ H.div [ A.classes ([B.inputGroup, Vc.searchInput] <> if state.search.valid then mempty else [B.hasError])]
                         [ H.input [ A.classes [B.formControl]
                                   , A.value state.search.value
                                   , A.title state.search.value
                                   , E.onFocus (E.input_ $ inj $ Focus true)
                                   , E.onBlur (E.input_ $ inj $ Focus false)
                                   , E.onInput (\v -> pure $ handler $ SearchChange state.search v state.path)
                                   ]
                                   []
                         , H.span [ A.class_ (if state.search.focused then Vc.searchPathActive else Vc.searchPath) ]
                                  [ H.span [ A.class_ Vc.searchPathBody ]
                                           [ H.text state.search.nextValue ]
                                  , H.span [ A.class_ (if state.search.nextValue == "" then Vc.searchAffixEmpty else Vc.searchAffix) ]
                                           [ H.text $ "path:" <> state.path ]
                                  ]
                         , H.img [ E.onClick (\_ -> pure $ handler $ SearchClear (state.searching && state.search.loading) state.search)
                                 , A.class_ Vc.searchClear
                                 , (if state.search.loading then A.src "img/spin.svg" else A.src "img/remove.svg")
                                 ]
                                 []
                         , H.span [ A.class_ B.inputGroupBtn ]
                                  [ H.button [ A.classes [B.btn, B.btnDefault]
                                             , A.disabled (not state.search.valid)
                                             ]
                                             [ glyph B.glyphiconSearch ]
                                  ]
                         ]
                 ]
        ]



sorting :: forall p e. (Request -> I e) -> State -> H.HTML p (I e)
sorting handler state =
  H.div [ A.classes [B.colXs4, Vc.toolbarSort] ]
        [ H.a (targetLink' $ handler $ SetSort $ notSort state.sort)
              [ H.text "Name"
              , H.i [ chevron state
                    , A.style (A.styles $ SM.fromList [Tuple "margin-left" "10px"])
                    ]
                    []
              ]
        ]
  where
  chevron { sort: Asc  } = A.classes [B.glyphicon, B.glyphiconChevronUp]
  chevron { sort: Desc } = A.classes [B.glyphicon, B.glyphiconChevronDown]

toolbar :: forall p e. (Request -> I e) -> State -> H.HTML p (I e)
toolbar handler state =
  H.div [ A.classes [B.colXs4, Vc.toolbarMenu] ]
        [ H.ul [ A.classes [B.listInline, B.pullRight] ]
               if inRoot state then [mount] else [file, folder, mount, notebook]
        ]
  where
  file :: H.HTML p (I e)
  file = H.li_ [ H.a [ A.href "javascript:void(0);"
                     , E.onClick (\ev -> pure $ handler $ UploadFile ev.target state)
                     ]
                     [ H.i [ A.title "upload file"
                           , A.classes [B.btnLg, B.glyphicon, B.glyphiconFile]
                           ]
                           [ H.input [ A.class_ B.hidden
                                     , A.type_ "file"
                                     , E.onChange (\ev -> pure $ handler $ FileListChanged ev.target state)
                                     ]
                                     []
                           ]
                     ]
               ]

  folder :: H.HTML p (I e)
  folder = toolItem' CreateFolder "create folder" B.glyphiconFolderClose

  notebook :: H.HTML p (I e)
  notebook = toolItem' CreateNotebook "create notebook" B.glyphiconBook

  mount :: H.HTML p (I e)
  mount = toolItem' MountDatabase "mount database" B.glyphiconHdd

  toolItem' :: (State -> Request) -> String -> A.ClassName -> H.HTML p (I e)
  toolItem' f = toolItem [B.btnLg] state (handler <<< f)

  inRoot :: State -> Boolean
  inRoot state = state.path == "" || state.path == "/"


toolItem :: forall a p e. [A.ClassName] -> a -> (a -> I e) -> String -> A.ClassName -> H.HTML p (I e)
toolItem classes actionArg action title icon =
  H.li_ [ H.a (targetLink' $ action actionArg)
              [ H.i [ A.title title
                    , A.classes (classes ++ [B.glyphicon, icon])
                    ]
                    []
              ]
        ]

item :: forall p e. (Request -> I e) -> Boolean -> Number -> Item -> H.HTML p (I e)
item handler searching ix state =
  H.div [ A.classes ([B.listGroupItem] ++ if state.selected then [B.listGroupItemInfo] else mempty)
        , E.onMouseOver (E.input_ $ inj $ ItemHover ix true)
        , E.onClick (E.input_ $ inj $ ItemSelect ix true)
        , E.onDoubleClick (\_ -> pure $ handler $ Open state)
        ]
        [ H.div [ A.class_ B.row ]
                [ H.div [ A.classes [B.colSm9, Vc.itemContent] ]
                        [ H.a (targetLink' $ handler $ Open state)
                              [ H.span_ [ H.i [ iconClasses state ]
                                              []
                                        , H.text $ (if searching then decodeURIPath state.root else "") <> state.name
                                        ]
                              ]
                        ]
                , H.div [ A.classes [B.colSm3, Vc.itemToolbar] ]
                        [ H.ul [ A.classes ([B.listInline, B.pullRight] ++ if not $ state.hovered || state.selected then [B.hidden] else mempty)
                               , A.style $ A.styles $ SM.fromList [Tuple "margin-bottom" "0"]
                               ]
                               (showToolbar state)
                        ]
                ]
        ]
  where

  iconClasses :: Item -> A.Attr (I e)
  iconClasses state = A.classes [B.glyphicon, Vc.itemIcon, iconClass state.resource]

  iconClass :: Resource -> A.ClassName
  iconClass File      = B.glyphiconFile
  iconClass Database  = B.glyphiconHdd
  iconClass Notebook  = B.glyphiconBook
  iconClass Directory = B.glyphiconFolderOpen
  iconClass Table     = B.glyphiconTh

  showToolbar item | item.name == up = []
                   | otherwise =
    let conf = case item.resource of
          Database -> [ H.li_ [ H.a (targetLink' $ handler $ Configure item)
                                       [ H.i [ A.title "configure"
                                             , A.classes [B.glyphicon, B.glyphiconWrench]
                                             ]
                                             []
                                       ]
                                 ]
                         ]
          _ -> []
    in conf <> [ toolItem' Move "move/rename" B.glyphiconMove
               , toolItem' Delete "remove" B.glyphiconTrash
               , toolItem' Share "share" B.glyphiconShare
               ]
    where
    toolItem' :: (Item -> Request) -> String -> A.ClassName -> H.HTML p (I e)
    toolItem' f = toolItem [] item (handler <<< f)

items :: forall p e. (Request -> I e) -> State -> H.HTML p (I e)
items handler state =
  H.div [ A.classes [B.listGroup, Vc.results] ]
        $ zipWith (item handler state.searching) (0..length state.items) state.items

view :: forall p e. (Request -> I e) -> State -> H.HTML p (I e)
view handler state =
  H.div_ [ navbar [ H.div [ A.classes [Vc.navCont, B.containerFluid] ]
                          [ icon, logo, search handler state ]
                  ]
         , content [ H.div [ A.class_ B.clearfix ]
                           [ breadcrumbs state.breadcrumbs
                           , toolbar handler state
                           ]
                   , row [ sorting handler state ]
                   , items handler state
                   ]
         , modal state
         ]
  where

  contentClasses :: [A.ClassName]
  contentClasses = [B.colMd8, B.colMdOffset2, B.colSm10, B.colSmOffset1]

  content :: [H.HTML p (I e)] -> H.HTML p (I e)
  content nodes = H.div [ A.class_ B.container ]
                        [ row [ H.div [ A.classes contentClasses ]
                                      nodes
                              ]
                        ]

  row :: [H.HTML p (I e)] -> H.HTML p (I e)
  row = H.div [ A.class_ B.row ]

  navbar :: [H.HTML p (I e)] -> H.HTML p (I e)
  navbar = H.nav [ A.classes [B.navbar, B.navbarInverse, B.navbarFixedTop] ]
