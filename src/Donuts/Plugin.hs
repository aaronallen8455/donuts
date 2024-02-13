module Donuts.Plugin
  ( plugin
  ) where

import qualified Donuts.Internal.GhcFacade as Ghc
import qualified Donuts.Internal.Plugin.Renamer as Renamer

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.renamedResultAction = const Renamer.renamedResultAction
  }
