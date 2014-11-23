module Model.Category where

import Prelude
import Database.Persist.TH

data Category = Onderbouw | Bovenbouw | Leraar deriving (Show,Read,Eq,Ord)

derivePersistField "Category"
