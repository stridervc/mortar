{-# LANGUAGE OverloadedStrings #-}

import SDL
import Control.Monad (unless, when)
import Control.Concurrent (threadDelay)

import Mortar

type SimpleState = Int

defaultState :: SimpleState
defaultState = 0

app :: App SimpleState
app = defaultApp  { appWidgets        = simpleWidgets
                  , appEventsHandler  = appEvents
                  , state             = defaultState
                  }

simpleWidgets :: SimpleState -> [Drawable]
simpleWidgets _ = [testWidget <+> testWidget <=> testWidget]

appEvents :: App SimpleState -> [Event] -> IO (App SimpleState)
appEvents a []      = return a
appEvents a (e:es)  = do
  a' <- appEvent a e
  appEvents a' es

appEvent :: App SimpleState -> Event -> IO (App SimpleState)
appEvent a e = appEventPayload a (eventPayload e)

appEventPayload :: App SimpleState -> EventPayload -> IO (App SimpleState)
appEventPayload a (WindowSizeChangedEvent d) = do
  let WindowSizeChangedEventData _ wh = d
  let V2 w h = wh
  return a { width = w, height = h }

appEventPayload a (WindowClosedEvent d) = do
  let WindowClosedEventData w = d
  destroyWindow w
  return a { appQuit = True }

-- TODO better key press handling, and destroyWindow on 'q'
appEventPayload a (KeyboardEvent d) = do
  if (keyboardEventKeyMotion d == Pressed && keysymKeycode (keyboardEventKeysym d) == KeycodeQ) then return a { appQuit = True } else return a

appEventPayload a p = do
  -- putStrLn $ show p
  return a

drawWidget :: Renderer -> Rectangle CInt -> Drawable -> IO ()
drawWidget r rect w = do
  rendererViewport r $= Just rect
  render r w

appLoop :: App SimpleState -> Renderer -> IO ()
appLoop a r = do
  event <- waitEvent  -- this blocks until the next event
  events <- pollEvents

  -- clear renderer
  rendererViewport r $= Nothing
  rendererDrawColor r $= V4 0 0 0 255
  clear r

  a' <- (appEventsHandler a) a $ event : events
  let w = CInt $ width a'
  let h = CInt $ height a'
  let rect = Rectangle (P (V2 0 0)) (V2 w h)
  mapM_ (drawWidget r rect) $ appWidgets a' (state a')

  present r
  threadDelay 16000 -- Is this necessary if we block on events?
  unless (appQuit a') (appLoop a' r)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Simple Mortar Example" defaultWindow { windowResizable = True }
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop app renderer

