{-# LANGUAGE DeriveGeneric #-}

module DisplayTypes where

import GHC.Generics
import Data.Aeson 
import qualified Data.Text as T 


-- |
-- Displayable takes a type and wraps it in a Display
-- Display will be used in HaskellDO to handle media rendering
class Displayable a where
  display :: a -> Display

-- |
-- DisplayType contains the allowed media to render 
data DisplayType
  = DisplayText
  | DisplayHtml
  | DisplaySVG
  | DisplayJS
  | DisplayImage 
  | DisplayList
  deriving Generic

-- |
-- The Display data type carries the type of data to render and the content to render
-- DisplayText is the default for things interpreted in GHCi
data Display = Display 
  { displayType :: DisplayType 
  , content :: String 
  }
  deriving Generic

instance ToJSON DisplayType
instance FromJSON DisplayType

instance ToJSON Display
instance FromJSON Display

-- |
-- Allows arbitrart Display types to be printed in the console
-- JSON was chosen for an easy format for the front-end of HaskellDO to parse
instance Show Display where
  show (Display DisplayList lst) = lst 
  show d = show $ toEncoding $ content d 