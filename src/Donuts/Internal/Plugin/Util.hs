module Donuts.Internal.Plugin.Util
  ( addApp
  , isAppOf
  ) where

import qualified Donuts.Internal.GhcFacade as Ghc

addApp :: Ghc.Name -> Ghc.HsExpr Ghc.GhcRn -> Ghc.HsExpr Ghc.GhcRn
addApp name expr
  = Ghc.HsApp
      Ghc.noComments
      (Ghc.noLocA (Ghc.HsVar Ghc.noExtField (Ghc.noLocA name)))
      (Ghc.noLocA expr)

isAppOf :: [Ghc.Name] -> Ghc.HsExpr Ghc.GhcRn -> Bool
isAppOf names = \case
  Ghc.HsVar _ (Ghc.L _ name) -> name `elem` names
  Ghc.HsPar _ _ inner _ -> isAppOf names (Ghc.unLoc inner)
  Ghc.OpApp _ (Ghc.L _ leftExpr)
              (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName)))
              _
    | fName == Ghc.dollarName -> isAppOf names leftExpr
    | otherwise -> fName `elem` names
        -- TODO handle strict dollar as well
  Ghc.HsApp _ (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName))) _argExpr
    -> fName `elem` names
  Ghc.HsApp _ fExpr _
    -> isAppOf names (Ghc.unLoc fExpr)
  _ -> False
