module Donuts.Internal.Plugin.StmtTransformer
  ( StmtTransformer(..)
  , transformEarlyReturn
  , transformLoop
  , transformMutVar
  , applyBodyTransformers
  , applyBindTransformers
  ) where

import qualified Donuts.Internal.GhcFacade as Ghc
import           Donuts.Internal.Plugin.Env (Env(..))
import           Donuts.Internal.Plugin.Util (addApp, isAppOf)

data StmtTransformer =
  MkStmtTransformer
    { transformBody
        :: Env
        -> Ghc.HsExpr Ghc.GhcRn
        -> Ghc.HsExpr Ghc.GhcRn
    , transformBindBody
        :: Env
        -> Ghc.HsExpr Ghc.GhcRn
        -> Ghc.HsExpr Ghc.GhcRn
    , bindVars
        :: Env
        -> [(Ghc.Name, Ghc.HsExpr Ghc.GhcRn)]
        -> [(Ghc.Name, Ghc.HsExpr Ghc.GhcRn)]
    }

transformEarlyReturn :: StmtTransformer
transformEarlyReturn = MkStmtTransformer
  { transformBody = \env body ->
      if isAppOf [ earlyReturnName env
                 , breakLName env
                 , continueLName env
                 , mutVarAssignOpName env
                 , newMutVarName env
                 , newMutVarStrictName env
                 ] body
      then body
      else addApp (liftName env) body
  , transformBindBody = addApp . liftName
  , bindVars = const id
  }

transformLoop :: StmtTransformer
transformLoop = MkStmtTransformer
  { transformBody = \env body ->
      if isAppOf [ breakLName env
                 , continueLName env
                 , newMutVarName env
                 , newMutVarStrictName env
                 , mutVarAssignOpName env
                 ] body
         then body
         else addApp (liftName env) body
  , transformBindBody = addApp . liftName
  , bindVars = fmap . fmap . addApp . liftName
  }

transformMutVar :: Bool -> Ghc.Name -> StmtTransformer
transformMutVar isStrict varName = MkStmtTransformer
  { transformBody = \env -> \case
      expr@(Ghc.OpApp _ (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ lName)))
                             (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ oName)))
                             rExpr)
        | oName == mutVarAssignOpName env
        , lName == varName
        -> -- asignment of this variable
          Ghc.HsApp Ghc.noComments
                    (Ghc.nlHsVar $
                      if isStrict
                         then setMutVarStrictName env
                         else setMutVarName env
                    )
                    rExpr
        | oName == mutVarAssignOpName env
        -> -- assignment for outer var.
           expr
      expr
        | isAppOf [ breakLName env
                  , continueLName env
                  , newMutVarName env
                  , newMutVarStrictName env
                  ] expr ->
          expr
        | otherwise ->
          addApp (liftName env) expr
  , transformBindBody = addApp . liftName
  , bindVars = \env bnds ->
      (varName, Ghc.nl_HsVar $ getMutVarName env)
       : (fmap (addApp $ liftName env) <$> bnds)
  }

applyBodyTransformers :: Env -> [StmtTransformer] -> Ghc.HsExpr Ghc.GhcRn -> Ghc.HsExpr Ghc.GhcRn
applyBodyTransformers env stmtTransformers st =
  foldr (`transformBody` env) st stmtTransformers

applyBindTransformers :: Env -> [StmtTransformer] -> Ghc.HsExpr Ghc.GhcRn -> Ghc.HsExpr Ghc.GhcRn
applyBindTransformers env stmtTransformers st =
  foldr (`transformBindBody` env) st stmtTransformers
