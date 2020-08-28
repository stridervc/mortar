module Mortar.Drawable
  ( Drawable (..)
  , GrowthPolicy (..)
  , WidgetD
  , ContainerD
  , Width
  , Height
  , dWPolicy
  , dHPolicy
  , dWidth
  , dHeight
  , widget
  , hBox
  , vBox
  , (<+>)
  , (<=>)
  , render
  ) where

import SDL
import Foreign.C.Types (CInt(..))
import Control.Monad (foldM, foldM_)

-- |A Drawable can be drawn to the screen, either a WidgetD or a ContainerD.
data Drawable     = Widget WidgetD | Container ContainerD
-- |GrowthPolicy determines whether a Drawable will have a fixed size, or grow to use as much
-- space as it's allowed.
data GrowthPolicy = Fixed | Grow deriving (Eq, Show, Ord)
-- |Direction indicates in which direction a ContainerD's children are lined up.
data Direction    = Horisontal | Vertical deriving (Eq,Show)

type Width   = CInt
type Height  = CInt

-- |Widget Drawable, the building block that can be drawn to the screen.
data WidgetD = WidgetD
  { renderWidget  :: Renderer -> IO ()  -- ^Function that renders the widget
  , wWPolicy      :: GrowthPolicy       -- ^Widget Width 'GrowthPolicy'
  , wHPolicy      :: GrowthPolicy       -- ^Widget Height 'GrowthPolicy'
  , wWidth        :: IO Width
  , wHeigt        :: IO Height
  }

-- container drawable
data ContainerD = ContainerD
  { drawables       :: [Drawable]
  , direction       :: Direction
  , cWPolicy        :: GrowthPolicy
  , cHPolicy        :: GrowthPolicy
  , cWidth          :: IO Width
  , cHeight         :: IO Height
  }

-- |The 'GrowthPolicy' for the width of a 'Drawable'
dWPolicy :: Drawable -> GrowthPolicy
dWPolicy (Widget d)     = wWPolicy d
dWPolicy (Container d)  = cWPolicy d

-- |The 'GrowthPolicy' for the height of a 'Drawable'
dHPolicy :: Drawable -> GrowthPolicy
dHPolicy (Widget d)     = wHPolicy d
dHPolicy (Container d)  = cHPolicy d

-- |The width of the 'Drawable'
dWidth :: Drawable -> IO Width
dWidth (Widget d)     = wWidth d
dWidth (Container d)  = cWidth d

-- |The height of the 'Drawable'
dHeight :: Drawable -> IO Height
dHeight (Widget d)    = wHeigt d
dHeight (Container d) = cHeight d

widget :: (IO Width,GrowthPolicy) -> (IO Height,GrowthPolicy) -> (Renderer -> IO ()) -> Drawable
widget (w,wp) (h,hp) f = Widget $ WidgetD
  { renderWidget  = f
  , wWPolicy      = wp
  , wHPolicy      = hp
  , wWidth        = w
  , wHeigt        = h
  }

hBox :: [Drawable] -> Drawable
hBox ds = Container $ ContainerD
  { drawables = ds
  , direction = Horisontal
  , cWPolicy  = maximum $ map dWPolicy ds
  , cHPolicy  = maximum $ map dHPolicy ds
  , cWidth    = do
    ws <- mapM dWidth ds
    return $ sum ws
  , cHeight   = do
    hs <- mapM dHeight ds
    return $ maximum hs
  }

vBox :: [Drawable] -> Drawable
vBox ds = Container $ ContainerD
  { drawables = ds
  , direction = Vertical
  , cWPolicy  = maximum $ map dWPolicy ds
  , cHPolicy  = maximum $ map dHPolicy ds
  , cWidth    = do
    ws <- mapM dWidth ds
    return $ maximum ws
  , cHeight   = do
    hs <- mapM dHeight ds
    return $ sum hs
  }

(<+>) :: Drawable -> Drawable -> Drawable
(<+>) da@(Widget a) db@(Widget b) = hBox [da,db]
(<+>) da@(Container a) db@(Widget b)
  | direction a == Horisontal = Container a { drawables = drawables a ++ [db] }
  | otherwise                 = hBox [da,db]
(<+>) da@(Widget a) db@(Container b)
  | direction b == Horisontal = Container b { drawables = da : drawables b }
  | otherwise                 = hBox [da,db]
(<+>) da@(Container a) db@(Container b)
  | direction a == Horisontal && direction b == Horisontal  = Container a { drawables = drawables a ++ drawables b }
  | otherwise                                               = hBox [da,db]

(<=>) :: Drawable -> Drawable -> Drawable
(<=>) da@(Widget a) db@(Widget b) = vBox [da,db]
(<=>) da@(Container a) db@(Widget b)
  | direction a == Vertical   = Container a { drawables = drawables a ++ [db] }
  | otherwise                 = vBox [da,db]
(<=>) da@(Widget a) db@(Container b)
  | direction b == Vertical   = Container b { drawables = da : drawables b }
  | otherwise                 = vBox [da,db]
(<=>) da@(Container a) db@(Container b)
  | direction a == Vertical && direction b == Vertical  = Container a { drawables = drawables a ++ drawables b }
  | otherwise                                           = vBox [da,db]

render :: Renderer -> Drawable -> IO ()
render r (Widget a) = renderWidget a r
render r c@(Container a)
  | direction a == Horisontal = renderHBox r c
  | direction a == Vertical   = renderVBox r c

-- render the child of an HBox at (x,y) with size (w,h)
-- Returns the (x,y) pos where the next child can be rendered
renderHBoxChild :: Renderer -> (CInt,CInt) -> ((IO Width,IO Height), Drawable) -> IO (CInt,CInt)
renderHBoxChild r (x,y) ((w,h),d) = do
  w' <- w
  h' <- h
  rendererViewport r $= Just (Rectangle (P (V2 x y)) (V2 w' h'))
  render r d
  return (x+w', y)

-- Render a horisontal box and its child widgets
-- First we ask Fixed size widgets how much space horisontally they need,
-- whatever space remains, is divided equally amongst the Growing widgets.
renderHBox :: Renderer -> Drawable -> IO ()
renderHBox r (Container a) = do
  viewport <- get $ rendererViewport r
  case viewport of
    Just (Rectangle (P (V2 x y)) (V2 w h)) -> do
      fixeddsw' <- mapM dWidth fixedds
      let fixeddsw  = sum fixeddsw'
      let growdsw   = (w-fixeddsw) `div` (toEnum (length growds))
      let dw        = (\d -> if dWPolicy d == Fixed then dWidth d else return growdsw)
      let dh        = (\d -> if dHPolicy d == Fixed then dHeight d else return h)
      let whds      = [((dw d, dh d), d) | d <- ds]
      foldM_ (renderHBoxChild r) (x,y) whds
  return ()
  where ds        = drawables a
        fixedds   = filter (\d -> dWPolicy d == Fixed) ds
        growds    = filter (\d -> dWPolicy d == Grow) ds

renderVBoxChild :: Renderer -> (CInt,CInt) -> ((IO Width,IO Height), Drawable) -> IO (CInt,CInt)
renderVBoxChild r (x,y) ((w,h),d) = do
  w' <- w
  h' <- h
  rendererViewport r $= Just (Rectangle (P (V2 x y)) (V2 w' h'))
  render r d
  return (x, y+h')

renderVBox :: Renderer -> Drawable -> IO ()
renderVBox r (Container a) = do
  viewport <- get $ rendererViewport r
  case viewport of
    Just (Rectangle (P (V2 x y)) (V2 w h)) -> do
      fixeddsh' <- mapM dHeight fixedds
      let fixeddsh  = sum fixeddsh'
      let growdsh   = (h-fixeddsh) `div` (toEnum (length growds))
      let dw        = (\d -> if dWPolicy d == Fixed then dWidth d else return w)
      let dh        = (\d -> if dHPolicy d == Fixed then dHeight d else return growdsh)
      let whds      = [((dw d, dh d), d) | d <- ds]
      foldM_ (renderVBoxChild r) (x,y) whds
  return ()
  where ds        = drawables a
        fixedds   = filter (\d -> dHPolicy d == Fixed) ds
        growds    = filter (\d -> dHPolicy d == Grow) ds
