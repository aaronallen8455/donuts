module Sugar.Internal.GhcFacade
  ( module Ghc
  ) where

import           GHC as Ghc
import           GHC.Driver.Plugins as Ghc
import           GHC.Tc.Types as Ghc hiding (TcPlugin, DefaultingPlugin)
import           GHC.Builtin.Names as Ghc
import           GHC.Types.Name.Occurrence as Ghc
import           GHC.Tc.Plugin as Ghc
