{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}

module DisplayFrames where 

import DisplayTypes
import Displayable
import Frames
import Data.Vinyl.TypeLevel (RecAll)
import Text.Blaze.Html5 (toHtml, td, tr)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Data.Text.Lazy (toChunks)
import qualified Data.Vinyl.Functor as V
import qualified Data.Text as T

-- instance of Displayable of Record from Frames package
instance (RecAll V.Identity (UnColumn r) Show, ColumnHeaders r, AsVinyl r) => Displayable (Record r) where
  display rec = Display DisplayHtml html 
    where 
      html = renderHtml $ toTable $ showFields rec
      toTable = tr . mapM_ (td . toHtml)
