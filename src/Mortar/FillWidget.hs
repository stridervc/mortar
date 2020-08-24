module Mortar.FillWidget
  ( hFill
  , vFill
  ) where

import Mortar.Drawable

hFill :: Drawable
hFill = widget (0,Grow) (0,Fixed) (\_ -> return ())

vFill :: Drawable
vFill = widget (0,Fixed) (0,Grow) (\_ -> return ())
