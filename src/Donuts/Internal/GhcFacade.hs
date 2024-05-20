{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
module Donuts.Internal.GhcFacade
  ( module Ghc
  , hsAppX
  , pattern HsPar'
  , pattern HsLam'
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

hsAppX :: Ghc.XApp Ghc.GhcRn
#if MIN_VERSION_ghc(9,10,0)
hsAppX = Ghc.noExtField
#else
hsAppX = Ghc.noComments
#endif

pattern HsPar'
  :: Ghc.XPar Ghc.GhcRn
#if MIN_VERSION_ghc(9,10,0)
  -> ()
#else
  -> Ghc.LHsToken "(" Ghc.GhcRn
#endif
  -> Ghc.LHsExpr Ghc.GhcRn
#if MIN_VERSION_ghc(9,10,0)
  -> ()
#else
  -> Ghc.LHsToken ")" Ghc.GhcRn
#endif
  -> Ghc.HsExpr Ghc.GhcRn
#if MIN_VERSION_ghc(9,10,0)
hsParShim :: x -> (x, (), ())
hsParShim x = (x, (), ())
pattern HsPar' x l expr r <- Ghc.HsPar (hsParShim -> (x, l, r)) expr
  where
    HsPar' x () expr () = Ghc.HsPar x expr
#else
pattern HsPar' x l expr r = Ghc.HsPar x l expr r
#endif

pattern HsLam'
  :: Ghc.XLam Ghc.GhcRn
#if MIN_VERSION_ghc(9,10,0)
  -> Ghc.HsLamVariant
#else
  -> ()
#endif
  -> Ghc.MatchGroup Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> Ghc.HsExpr Ghc.GhcRn
#if MIN_VERSION_ghc(9,10,0)
pattern HsLam' x v mg = Ghc.HsLam x v mg
#else
hsLamShim :: x -> (x, ())
hsLamShim x = (x, ())
pattern HsLam' x v mg <- Ghc.HsLam (hsLamShim -> (x, v)) mg
  where
    HsLam' x () mg = Ghc.HsLam x mg
#endif
