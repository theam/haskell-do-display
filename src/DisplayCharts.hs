module DisplayCharts where 

import Displayable
import DisplayTypes
import System.IO.Unsafe (unsafePerformIO)
import Data.Default.Class
import System.Directory 
import System.FilePath (pathSeparator)
import Graphics.Rendering.Chart.Renderable 
import Graphics.Rendering.Chart.Backend.Diagrams 

-- instance for Chart from renderables
instance Displayable (Renderable a) where 
  display r = unsafePerformIO ret 
    where 
      ret = do 
        fp <- storeChart r 
        return $ Display DisplaySVG fp 

-- Chart helper functions for storing and retrieving FilePath
tmpDir :: IO () 
tmpDir = createDirectoryIfMissing False ".tmpImages"
  
storeChart :: Renderable a -> IO FilePath 
storeChart c = do 
  tmpDir 
  let filename = ".haskell-do-chart.svg"
  renderableToFile def filename c
  return $ ".tmpImages" ++ [pathSeparator] ++ filename