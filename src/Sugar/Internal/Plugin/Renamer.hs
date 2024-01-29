module Sugar.Internal.Plugin.Renamer
  ( renamedResultAction
  ) where

import           Data.Data (Data)
import qualified Data.Data as Data
import           Data.Maybe

import qualified Sugar.Internal.GhcFacade as Ghc

data Env = MkEnv
  { earlyReturnName :: Ghc.Name
  , earlyReturnWrapDoName :: Ghc.Name
  , forLoopName :: Ghc.Name
--   , repeatLoopName :: Ghc.Name
--   , whileLoopName :: Ghc.Name
  , liftName :: Ghc.Name
  , voidName :: Ghc.Name
  }

renamedResultAction
  :: Ghc.TcGblEnv
  -> Ghc.HsGroup Ghc.GhcRn
  -> Ghc.TcM (Ghc.TcGblEnv, Ghc.HsGroup Ghc.GhcRn)
renamedResultAction gblEnv group = do
  Ghc.Found _ sugarMod <-
    Ghc.runTcPluginM $
      Ghc.findImportedModule (Ghc.mkModuleName "Sugar.Api") Ghc.NoPkgQual

  env <- Ghc.runTcPluginM $ MkEnv
    <$> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "earlyReturn")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "earlyReturnWrapDo")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "forLoop")
--     <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "repeatLoop")
--     <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "whileLoop")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "lift")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "void")
  pure (gblEnv, transform env group)

newtype T a = T (a -> Maybe a)

transform :: Data a => Env -> a -> a
transform env x =
  let recurse = Data.gmapT (transform env) x
   in case Data.gcast (T checkAndApply) of
      Nothing -> recurse
      Just (T f) -> fromMaybe recurse $ f x
  where
    checkAndApply = \case
      Ghc.HsDo m (Ghc.DoExpr Nothing) (Ghc.L loc stmts)
        | any (isEarlyReturnStmt env . Ghc.unLoc) stmts ->
          -- run monad transformer
           let newStmts = Ghc.L loc $ map (transformStmt env) stmts
            in Just $
              Ghc.HsApp Ghc.noComments
                (Ghc.noLocA (Ghc.HsVar Ghc.noExtField
                  (Ghc.noLocA $ earlyReturnWrapDoName env)))
              (Ghc.noLocA $ Ghc.HsDo m (Ghc.DoExpr Nothing) newStmts)
      _ -> Nothing

transformStmt :: Env -> Ghc.ExprLStmt Ghc.GhcRn -> Ghc.ExprLStmt Ghc.GhcRn
transformStmt env (Ghc.L loc stmt) = Ghc.L loc $ case stmt of
  Ghc.BodyStmt x body@(Ghc.L _ b) syn1 syn2 | isAppOf [earlyReturnName env] b ->
    Ghc.BodyStmt x (addApp (voidName env) body) syn1 syn2
  -- _ | isEarlyReturnStmt env stmt -> Ghc.L loc stmt
  Ghc.BindStmt x pat body ->
    Ghc.BindStmt x pat (addApp (liftName env) $ transform env body)
  Ghc.BodyStmt x (Ghc.L bl (Ghc.HsIf ix predi t e)) syn1 syn2 ->
    Ghc.BodyStmt x (Ghc.L bl (Ghc.HsIf ix predi (transformExpr t) (transformExpr e))) syn1 syn2
  Ghc.BodyStmt x body syn1 syn2 ->
    Ghc.BodyStmt x (addApp (liftName env) $ transform env body) syn1 syn2
  Ghc.LastStmt x body stripped syn ->
    Ghc.LastStmt x (addApp (liftName env) $ transform env body) stripped syn
  -- TODO recurse into loops
  _ -> transform env stmt
  where
    addApp :: Ghc.Name -> Ghc.LHsExpr Ghc.GhcRn -> Ghc.LHsExpr Ghc.GhcRn
    addApp name
      = Ghc.noLocA
      . Ghc.HsApp
          Ghc.noComments
          (Ghc.noLocA (Ghc.HsVar Ghc.noExtField (Ghc.noLocA name)))

    transformExpr :: Ghc.LHsExpr Ghc.GhcRn -> Ghc.LHsExpr Ghc.GhcRn
    transformExpr = \case
      Ghc.L loc2 (Ghc.HsDo m (Ghc.DoExpr Nothing) (Ghc.L sloc stmts)) ->
        let newStmts = Ghc.L sloc $ map (transformStmt env) stmts
         in Ghc.L loc2 $ Ghc.HsDo m (Ghc.DoExpr Nothing) newStmts
      expr | isAppOf [earlyReturnName env] (Ghc.unLoc expr) -> expr
      expr -> addApp (liftName env) expr

isEarlyReturnStmt
  :: Env
  -> Ghc.StmtLR Ghc.GhcRn Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> Bool
isEarlyReturnStmt env = \case
  Ghc.LastStmt _ body _ _ -> isEarlyReturn (Ghc.unLoc body)
  Ghc.BodyStmt _ body _ _ -> isEarlyReturn (Ghc.unLoc body)
  -- Should bound if statements be transformed? Probably not since a bind
  -- implies opening a new scope.
  -- Ghc.BindStmt _ _pat body -> isIfThenElseWithEarlyReturn (Ghc.unLoc body)
  _ -> False
  where
    loopNames =
      [ forLoopName env
--       , repeatLoopName env
--       , whileLoopName env
      ]
    isEarlyReturn expr =
      -- look for application of earlyReturn or forLoop with a do block argument.
      -- This might involve looking through applications of $ and parens.
      isAppOf [earlyReturnName env] expr
      || (isAppOf loopNames expr && isDoBlockArgWithEarlyReturn expr)
      -- TODO other loop types + when
      || isIfThenElseWithEarlyReturn expr
    isDoBlockArgWithEarlyReturn = \case
      Ghc.HsDo _ (Ghc.DoExpr Nothing) (Ghc.L _ stmts)
        -> any (isEarlyReturnStmt env . Ghc.unLoc) stmts
      _ -> False
    isIfThenElseWithEarlyReturn = \case
      Ghc.HsIf _ _pred (Ghc.L _ t) (Ghc.L _ e) ->
        isDoBlockArgWithEarlyReturn t || isEarlyReturn t
        || isDoBlockArgWithEarlyReturn e || isEarlyReturn e
      _ -> False

isAppOf :: [Ghc.Name] -> Ghc.HsExpr Ghc.GhcRn -> Bool
isAppOf names = \case
  Ghc.HsPar _ _ inner _ -> isAppOf names (Ghc.unLoc inner)
  Ghc.HsApp _ (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName)))
              (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ argName)))
    -> fName == Ghc.dollarName && argName `elem` names
  Ghc.HsApp _ (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName))) _argExpr
    -> fName `elem` names
  Ghc.HsApp _ fExpr _
    -> isAppOf names (Ghc.unLoc fExpr)
  _ -> False

-- It may be necessary to remove 'mut' and all the mut var names from the free
-- variables list of the do expr in which they occur. The renamer creates these
-- lists so they are expected to be there. Seems like this list goes into the
-- extension field of the Pattern constructor. What is it used for?
-- Could probably work around this not hijacking let bindings and instead have
-- something like `letMut foo := ...`

-- need a generic bottom up traversal that stops at do blocks and checks if
-- any of the statements meet some criteria and if so, only continue the traversal
-- in statements that are not sugared loops.
--
-- Transform each statement individually and also return something that indicates
-- what wrapper needs to be placed at the head of the do block?
