module Mortar.Label
  ( label
  ) where

import SDL
import SDL.Font
import Mortar.Drawable
import Data.Text

label :: Text -> Drawable
label t = widget (0,Grow) (70,Fixed) (drawLabel t)

{-
labelWidth :: Text -> CInt

labelHeight :: Text -> CInt
-}

drawLabel :: Text -> Renderer -> IO ()
drawLabel t r = do
  viewport <- get $rendererViewport r
  case viewport of
    Just (Rectangle _ (V2 w h)) -> do
      rendererDrawColor r $= V4 0 255 0 255
      drawLine r (P (V2 0 0)) (P (V2 w h))

      font <- SDL.Font.load "/usr/share/fonts/truetype/roboto/slab/RobotoSlab-Regular.ttf" 70
      surface <- SDL.Font.solid font (V4 255 255 255 0) t
      SDL.Font.free font
      texture <- createTextureFromSurface r surface
      
      SDL.copy r texture Nothing Nothing

      freeSurface surface
      destroyTexture texture
