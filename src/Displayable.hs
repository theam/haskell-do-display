module Displayable where 

import DisplayTypes 
import Data.Functor.Identity
import qualified Data.Text as T

-- Instances which should just be displayed by the console as usual
instance Displayable Bool where 
  display b = Display DisplayText (show b)

instance Displayable Int where 
  display i = Display DisplayText (show i)

instance Displayable Float where 
  display f = Display DisplayText (show f)

instance Displayable Double where
  display d = Display DisplayText (show d)

instance Displayable Char where 
  display c = Display DisplayText (show c)

instance Displayable T.Text where 
  display t = Display DisplayText (T.unpack t)

instance (Show a) => Displayable [a] where 
  display lst = Display DisplayText (show lst)

instance (Show a) => Displayable (Maybe a) where 
  display m = Display DisplayText (show m)

instance (Show a) => Displayable (Identity a) where 
  display i = Display DisplayText (show i)

instance (Show a, Show e) => Displayable (Either e a) where
  display e = Display DisplayText (show e)

instance Displayable () where 
  display () = Display DisplayText "()"

-- Thar be tuples here
instance (Show a, Show b) => Displayable (a, b) where 
  display tup = Display DisplayText (show tup)

instance (Show a, Show b, Show c) => Displayable (a, b, c) where 
  display tup = Display DisplayText (show tup)

instance (Show a, Show b, Show c, Show d) => Displayable (a, b, c, d) where 
  display tup = Display DisplayText (show tup)

instance (Show a, Show b, Show c, Show d, Show e) => Displayable (a, b, c, d, e) where 
  display tup = Display DisplayText (show tup)

instance (Show a, Show b, Show c, Show d, Show e, Show f) => Displayable (a, b, c, d, e, f) where 
  display tup = Display DisplayText (show tup)

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Displayable (a, b, c, d, e, f, g) where 
  display tup = Display DisplayText (show tup)

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => Displayable (a, b, c, d, e, f, g, h) where 
  display tup = Display DisplayText (show tup)

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) => Displayable (a, b, c, d, e, f, g, h, i) where 
  display tup = Display DisplayText (show tup)

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j) => Displayable (a, b, c, d, e, f, g, h, i, j) where 
  display tup = Display DisplayText (show tup)

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k) => Displayable (a, b, c, d, e, f, g, h, i, j, k) where 
  display tup = Display DisplayText (show tup)

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l) => Displayable (a, b, c, d, e, f, g, h, i, j, k, l) where 
  display tup = Display DisplayText (show tup)

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m) => Displayable (a, b, c, d, e, f, g, h, i, j, k, l, m) where 
  display tup = Display DisplayText (show tup)

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n) => Displayable (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where 
  display tup = Display DisplayText (show tup)

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o) => Displayable (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where 
  display tup = Display DisplayText (show tup)

