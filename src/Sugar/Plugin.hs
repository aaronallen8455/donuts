module Sugar.Plugin
  ( plugin
  ) where

import qualified Sugar.Internal.GhcFacade as Ghc
import qualified Sugar.Internal.Plugin.Renamer as Renamer

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.renamedResultAction = const Renamer.renamedResultAction
  }
