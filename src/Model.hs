module Model where

import Data.Char(isAlphaNum, isLower)
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Prelude

import qualified Data.Text as T

type URL = Text


newtype TagText = TagText { unTT :: Text } deriving (Eq,Ord,Typeable,PersistField)

instance Show TagText where
  show (TagText t) = show t

instance Read TagText where
  readsPrec i = map (\(t,s) -> (TagText t,s)) . readsPrec i

instance PathPiece TagText where
  toPathPiece (TagText t) = toPathPiece t
  fromPathPiece = either (const Nothing) Just . tagText


tagText               :: Text -> Either Text TagText
tagText s
  | T.all isTagChar s = Right . TagText $ s
  | otherwise         = Left "Invalid Tag"

isTagChar   :: Char -> Bool
isTagChar c = isLower c && isAlphaNum c


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


type NodeId = DBNodeId

instance Eq Tag where
  (Tag t) == (Tag t') = t == t'

instance Ord Tag where
  (Tag t) <= (Tag t') = t <= t'
