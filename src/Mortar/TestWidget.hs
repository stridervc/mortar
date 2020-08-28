module Mortar.TestWidget
  ( testWidget
  ) where

import SDL
import Mortar.Drawable

testWidget :: Drawable
testWidget = widget (return 0,Grow) (return 0,Grow) drawTestWidget

drawTestWidget :: Renderer -> IO ()
drawTestWidget r = do
  viewport <- get $ rendererViewport r
  case viewport of
    Just (Rectangle _ (V2 w h)) -> do
      -- fillRect r $ Just (Rectangle (P (V2 0 0)) (V2 w h))
      rendererDrawColor r $= V4 255 0 0 255
      drawLine r (P (V2 0 0)) (P (V2 w h))
      drawLine r (P (V2 w 0)) (P (V2 0 h))

      rendererDrawColor r $= V4 255 255 255 255
      drawRect r $ Just $ Rectangle (P (V2 0 0)) (V2 w h)
