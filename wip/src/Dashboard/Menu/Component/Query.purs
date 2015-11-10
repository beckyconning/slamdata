module Dashboard.Menu.Component.Query where

import Prelude

import Halogen.Menu.Component as HalogenMenu

import Notebook.Component as Notebook

type QueryP = HalogenMenu.MenuQueryP (Notebook.NotebookQuery Unit)

