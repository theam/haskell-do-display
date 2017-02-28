{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}

module DisplayFrames where 

import DisplayTypes
import Displayable
import Frames
import Data.Vinyl.TypeLevel (RecAll)
import Text.Blaze.Html5 (toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text.Lazy (toChunks)
import qualified Data.Vinyl.Functor as V
import qualified Data.Text as T

-- instance of Displayable of Record from Frames package
instance (RecAll V.Identity r Show, ColumnHeaders r, AsVinyl r) => Displayable (Record r) where
  display rec = Display DisplayHtml html 
    where 
      html = (T.unpack . T.concat . toChunks . renderHtml) (toHtml $ show rec)
