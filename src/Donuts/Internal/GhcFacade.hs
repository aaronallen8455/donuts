{-# LANGUAGE CPP #-}
module Donuts.Internal.GhcFacade
  ( module Ghc
  ) where

#if MIN_VERSION_ghc(9,8,0)
import           GHC as Ghc
import           GHC.Driver.Plugins as Ghc
import           GHC.Tc.Types as Ghc hiding (TcPlugin, DefaultingPlugin)
import           GHC.Builtin.Names as Ghc
import           GHC.Types.Name.Occurrence as Ghc
import           GHC.Tc.Plugin as Ghc
import           GHC.Utils.Outputable as Ghc
import           GHC.Types.Name as Ghc
import           GHC.Data.Bag as Ghc
import           GHC.Driver.DynFlags as Ghc

#elif MIN_VERSION_ghc(9,4,0)
import           GHC as Ghc
import           GHC.Driver.Plugins as Ghc
import           GHC.Tc.Types as Ghc hiding (TcPlugin, DefaultingPlugin)
import           GHC.Builtin.Names as Ghc
import           GHC.Types.Name.Occurrence as Ghc
import           GHC.Tc.Plugin as Ghc
import           GHC.Utils.Outputable as Ghc
import           GHC.Types.Name as Ghc
import           GHC.Data.Bag as Ghc
import           GHC.Driver.Session as Ghc

#endif
