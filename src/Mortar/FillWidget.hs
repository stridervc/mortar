module Mortar.FillWidget
  ( hFill
  , vFill
  ) where

import Mortar.Drawable

hFill :: Drawable
hFill = widget (return 0,Grow) (return 0,Fixed) (\_ -> return ())

vFill :: Drawable
vFill = widget (return 0,Fixed) (return 0,Grow) (\_ -> return ())
