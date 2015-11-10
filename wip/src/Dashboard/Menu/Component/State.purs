module Dashboard.Menu.Component.State where

import Prelude

import Halogen.Menu.Model as HalogenMenu
import Halogen.Menu.Component as HalogenMenu

import Notebook.Cell.CellType (CellType(..))
import Notebook.Component as Notebook

type StateP g = HalogenMenu.MenuP (Notebook.NotebookQuery Unit) g

initial :: HalogenMenu.Menu (Notebook.NotebookQuery Unit)
initial = HalogenMenu.makeMenu
  [ { label: "Notebook"
    , submenu:
        [ { label: "Rename/Move"
          , value: Notebook.AddCell Explore unit
          }
        , { label: "Delete"
          , value: Notebook.AddCell Explore unit
          }
        , { label: "Publish"
          , value: Notebook.AddCell Explore unit
          }
        ]
    }
  , { label: "Insert"
    , submenu:
        [ { label: "Query"
          , value: Notebook.AddCell Explore unit
          }
        , { label: "Markdown"
          , value: Notebook.AddCell Explore unit
          }
        , { label: "Explore unit"
          , value: Notebook.AddCell Explore unit
          }
        , { label: "Search"
          , value: Notebook.AddCell Explore unit
          }
        ]
    }
  , { label: "Cell"
    , submenu:
        [ { label: "Evaluate"
          , value: Notebook.AddCell Explore unit
          }
        , { label: "Delete"
          , value: Notebook.AddCell Explore unit
          }
        ]
    }
  , { label: "Help"
    , submenu:
        [ { label: "Tutorial"
          , value: Notebook.AddCell Explore unit
          }
        , { label: "SQL Tutorial"
          , value: Notebook.AddCell Explore unit
          }
        , { label: "SQL Reference"
          , value: Notebook.AddCell Explore unit
          }
        , { label: "Report bug"
          , value: Notebook.AddCell Explore unit
          }
        , { label: "Request support"
          , value: Notebook.AddCell Explore unit
          }
        ]
    }
  ]

