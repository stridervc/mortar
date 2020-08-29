module Mortar.Image
  ( image
  ) where

import SDL
import qualified SDL.Image as I
import Mortar.Drawable

image :: FilePath -> Width -> Height -> Drawable
image f w h = widget (return w,Fixed) (return h,Fixed) (drawImage f)

drawImage :: FilePath -> Renderer -> IO ()
drawImage f r = do
  texture <- I.loadTexture r f
  viewport <- get $rendererViewport r
  case viewport of
    Just (Rectangle _ (V2 w h)) -> do
      SDL.copy r texture Nothing Nothing
      destroyTexture texture
