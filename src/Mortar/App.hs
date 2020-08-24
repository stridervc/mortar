{-# LANGUAGE OverloadedStrings #-}

module Mortar.App
  ( App (..)
  , defaultApp
  ) where

import SDL
import GHC.Int (Int32)

import Mortar.Drawable

data App s = App  { appWidgets        :: s -> [Drawable]
                  , appEventsHandler  :: App s -> [Event] -> IO (App s)
                  , width             :: Int32
                  , height            :: Int32
                  , appQuit           :: Bool
                  , state             :: s
                  }

defaultApp :: App s
defaultApp = App  { width   = 0
                  , height  = 0
                  , appQuit = False
                  }
