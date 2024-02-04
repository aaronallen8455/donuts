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
  , whenName :: Ghc.Name
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
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "when")
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
           let newStmts = Ghc.L loc $ map (transformStmt env) stmts
            in Just $
              Ghc.HsApp Ghc.noComments
                (Ghc.noLocA (Ghc.HsVar Ghc.noExtField
                  (Ghc.noLocA $ earlyReturnWrapDoName env)))
              (Ghc.noLocA $ Ghc.HsDo m (Ghc.DoExpr Nothing) newStmts)
      _ -> Nothing

transformStmt :: Env -> Ghc.ExprLStmt Ghc.GhcRn -> Ghc.ExprLStmt Ghc.GhcRn
transformStmt env (Ghc.L loc stmt) = Ghc.L loc $ case stmt of
  Ghc.BodyStmt x body syn1 syn2 ->
    Ghc.BodyStmt x (transformBodyStmt body) syn1 syn2
  Ghc.BindStmt x pat body ->
    Ghc.BindStmt x pat (addApp (liftName env) $ transform env body)
  Ghc.LastStmt x body@(Ghc.L _ b) stripped syn | isAppOf [earlyReturnName env] b ->
    Ghc.LastStmt x (transform env body) stripped syn
  Ghc.LastStmt x body stripped syn ->
    Ghc.LastStmt x (transformBodyStmt body) stripped syn
  _ -> transform env stmt
  where
    addApp :: Ghc.Name -> Ghc.LHsExpr Ghc.GhcRn -> Ghc.LHsExpr Ghc.GhcRn
    addApp name
      = Ghc.noLocA
      . Ghc.HsApp
          Ghc.noComments
          (Ghc.noLocA (Ghc.HsVar Ghc.noExtField (Ghc.noLocA name)))

    transformBodyStmt = \case
      body@(Ghc.L _ b) | isAppOf [earlyReturnName env] b ->
        addApp (voidName env) $ transform env body
      body | isAppOf loopNames (Ghc.unLoc body) ->
        transformDoArg body
      Ghc.L bl (Ghc.HsIf ix predi t e) ->
        Ghc.L bl (Ghc.HsIf ix predi (transformExpr t) (transformExpr e))
      Ghc.L bL (Ghc.HsCase cx scrut mg) ->
        Ghc.L bL (Ghc.HsCase cx (transform env scrut) $ transformMatchGroup mg)
      body ->
        addApp (liftName env) $ transform env body

    transformMatchGroup mg = mg { Ghc.mg_alts = map (fmap transformMatch) <$> Ghc.mg_alts mg }
    transformMatch match =
      match { Ghc.m_pats = transform env <$> Ghc.m_pats match
            , Ghc.m_grhss = (Ghc.m_grhss match)
              { Ghc.grhssGRHSs =
                  map (fmap transformGRHS) (Ghc.grhssGRHSs (Ghc.m_grhss match))
              }
            }
    transformGRHS (Ghc.GRHS x guards body) =
      Ghc.GRHS x
        (transform env <$> guards)
        (transformDoArg body)

    -- how is this different from transformDoArg?
    transformExpr :: Ghc.LHsExpr Ghc.GhcRn -> Ghc.LHsExpr Ghc.GhcRn
    transformExpr = \case
      Ghc.L loc2 (Ghc.HsPar x a inner b) ->
        Ghc.L loc2 (Ghc.HsPar x a (transformExpr inner) b)
      Ghc.L loc2 (Ghc.HsDo m (Ghc.DoExpr Nothing) stmts) ->
        let newStmts = map (transformStmt env) <$> stmts
         in Ghc.L loc2 $ Ghc.HsDo m (Ghc.DoExpr Nothing) newStmts
      expr | isAppOf [earlyReturnName env] (Ghc.unLoc expr) -> expr
      expr -> addApp (liftName env) expr

    transformDoArg (Ghc.L eL expr) = case expr of
      Ghc.HsLam lX mg ->
        Ghc.L eL $ Ghc.HsLam lX (transformMatchGroup mg)
      Ghc.HsPar x a inner b ->
        Ghc.L eL $ Ghc.HsPar x a (transformDoArg inner) b
      Ghc.OpApp x lExpr oExpr rExpr -> -- TODO check lExpr
        Ghc.L eL . Ghc.OpApp x (transform env lExpr) oExpr $ transformDoArg rExpr
      Ghc.HsApp x fExpr@(Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName))) aExpr
        | fName == earlyReturnName env ->
          {-addApp (voidName env) .-} Ghc.L eL $ Ghc.HsApp x fExpr (transform env aExpr)
      Ghc.HsApp x fExpr aExpr -> -- TODO check fExpr
        Ghc.L eL . Ghc.HsApp x (transform env fExpr) $ transformDoArg aExpr
      Ghc.HsDo m (Ghc.DoExpr Nothing) stmts ->
        let newStmts = map (transformStmt env) <$> stmts
         in Ghc.L eL $ Ghc.HsDo m (Ghc.DoExpr Nothing) newStmts
      x -> transformExpr $ Ghc.L eL x

    loopNames = getLoopNames env

getLoopNames :: Env -> [Ghc.Name]
getLoopNames env =
  [ forLoopName env
  , whenName env
--  , repeatLoopName env
--  , whileLoopName env
  ]

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
    loopNames = getLoopNames env
    isEarlyReturn expr =
      -- look for application of earlyReturn or forLoop with a do block argument.
      -- This might involve looking through applications of $ and parens.
      isAppOf [earlyReturnName env] expr
        -- Lump loops in with early return for now
      || isAppOf loopNames expr -- && isDoBlockArgWithEarlyReturn expr)
      -- TODO other loop types
      -- TODO dollar bang
      || isIfThenElseWithEarlyReturn expr
      || isCaseWithEarlyRet expr
    isDoBlockArgWithEarlyReturn = \case
      Ghc.HsPar _ _ inner _ -> isDoBlockArgWithEarlyReturn $ Ghc.unLoc inner
      Ghc.HsApp _ expr _ | isAppOf [earlyReturnName env] $ Ghc.unLoc expr -> True
      Ghc.HsApp _ fExpr arg -> isAppOf loopNames (Ghc.unLoc fExpr)
                            && isDoBlockArgWithEarlyReturn (Ghc.unLoc arg)
      Ghc.OpApp _ (Ghc.L _ leftExpr)
                  (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName)))
                  (Ghc.L _ rightExpr)
        -> isAppOf loopNames leftExpr
           && fName == Ghc.dollarName
           && isDoBlockArgWithEarlyReturn rightExpr
      Ghc.HsDo _ (Ghc.DoExpr Nothing) (Ghc.L _ stmts)
        -> any (isEarlyReturnStmt env . Ghc.unLoc) stmts
      _ -> False
    isIfThenElseWithEarlyReturn = \case
      Ghc.HsIf _ _pred (Ghc.L _ t) (Ghc.L _ e) ->
        isDoBlockArgWithEarlyReturn t || isEarlyReturn t
        || isDoBlockArgWithEarlyReturn e || isEarlyReturn e
      _ -> False
    isCaseWithEarlyRet = \case
      Ghc.HsCase _ _ mg -> any (matchWithEarlyRet . Ghc.unLoc) (Ghc.unLoc $ Ghc.mg_alts mg)
      _ -> False
    matchWithEarlyRet = any (grhsWithEarlyRet . Ghc.unLoc) . Ghc.grhssGRHSs . Ghc.m_grhss
    grhsWithEarlyRet (Ghc.GRHS _ _guards body) = isEarlyReturn (Ghc.unLoc body)

isAppOf :: [Ghc.Name] -> Ghc.HsExpr Ghc.GhcRn -> Bool
isAppOf names = \case
  Ghc.HsVar _ (Ghc.L _ name) -> name `elem` names
  Ghc.HsPar _ _ inner _ -> isAppOf names (Ghc.unLoc inner)
  Ghc.OpApp _ (Ghc.L _ leftExpr)
              (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName)))
              _
    -> fName == Ghc.dollarName && isAppOf names leftExpr
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
