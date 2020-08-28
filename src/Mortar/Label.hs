module Mortar.Label
  ( label
  ) where

import SDL
import SDL.Font
import Mortar.Drawable
import Data.Text
import Foreign.C.Types (CInt(..))

label :: Font -> Color -> Color -> Text -> Drawable
label f fg bg t = widget (labelWidth f t,Fixed) (labelHeight f t,Fixed) (drawLabel f fg bg t)

labelWidth :: Font -> Text -> IO CInt
labelWidth f t = do
  (w,_) <- size f t
  return $ toEnum w

labelHeight :: Font -> Text -> IO CInt
labelHeight f t = do
  (_,h) <- size f t
  return $ toEnum h

drawLabel :: Font -> Color -> Color -> Text -> Renderer -> IO ()
drawLabel font fg bg t r = do
  viewport <- get $rendererViewport r
  case viewport of
    Just (Rectangle _ (V2 w h)) -> do
      rendererDrawColor r $= V4 0 255 0 255

      surface <- SDL.Font.shaded font fg bg t
      texture <- createTextureFromSurface r surface

      SDL.copy r texture Nothing Nothing

      freeSurface surface
      destroyTexture texture
