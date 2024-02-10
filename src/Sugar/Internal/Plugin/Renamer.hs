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
  , repeatLName :: Ghc.Name
  , continueLName :: Ghc.Name
  , breakLName :: Ghc.Name
--   , repeatLoopName :: Ghc.Name
--   , whileLoopName :: Ghc.Name
  , liftName :: Ghc.Name
--   , voidName :: Ghc.Name
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
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "repeatL")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "continueL")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "breakL")
--     <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "repeatLoop")
--     <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "whileLoop")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "lift")
--     <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "void")
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
        | any (isTargetStmt env . Ghc.unLoc) stmts ->
           let newStmts = Ghc.L loc $ map (transformStmt env [transformEarlyReturn]) stmts
            in Just $
              Ghc.HsApp Ghc.noComments
                (Ghc.noLocA (Ghc.HsVar Ghc.noExtField
                  (Ghc.noLocA $ earlyReturnWrapDoName env)))
              (Ghc.noLocA $ Ghc.HsDo m (Ghc.DoExpr Nothing) newStmts)
      _ -> Nothing

-- transformStmt should only deal with traversing the list of do statements.
-- It should accumulate a list of transformer functions that are applied in
-- order to each statement, possibly producing multiple statements which are
-- in turn passed to each subsequent transformer function.
-- It's important that these concerns are delineated so that transformations
-- can be layered on top of each other.
-- It needs to be able to look into the do expressions inside of the various
-- loop combinators, the individual transformers should not be concerned with that.
-- Should also look into if and case statements.
-- For convenience, the early return transformation will always be at the
-- bottom of the stack and will apply the 'transform' function at appropriate
-- points.
-- Should not pass statements with control function apps to transformer functions
-- transformStmt should handle recursive calls of transform, after checking
-- for control functions.

data Stmt
  = Body (Ghc.HsExpr Ghc.GhcRn)
  | Bind (Ghc.LPat Ghc.GhcRn) (Ghc.HsExpr Ghc.GhcRn)

type StmtTransformer = Env -> Stmt -> Stmt

transformEarlyReturn :: StmtTransformer
transformEarlyReturn env = \case
  Body body -> Body $ transformBodyStmt body
  Bind pat body -> Bind pat $ addApp (liftName env) body
  where
    transformBodyStmt = \case
      body | isAppOf [earlyReturnName env, breakLName env, continueLName env] body -> body
      body -> addApp (liftName env) body

transformLoop :: StmtTransformer
transformLoop env = \case
  Body body
    | isAppOf [breakLName env, continueLName env] body -> Body body
    | otherwise -> Body $ addApp (liftName env) body
  Bind pat body -> Bind pat $ addApp (liftName env) body

addApp :: Ghc.Name -> Ghc.HsExpr Ghc.GhcRn -> Ghc.HsExpr Ghc.GhcRn
addApp name expr
  = Ghc.HsApp
      Ghc.noComments
      (Ghc.noLocA (Ghc.HsVar Ghc.noExtField (Ghc.noLocA name)))
      (Ghc.noLocA expr)

defaultThenExpr :: Ghc.HsExpr Ghc.GhcRn
defaultThenExpr = Ghc.HsVar Ghc.noExtField (Ghc.noLocA Ghc.thenMName)

defaultBindExpr :: Ghc.HsExpr Ghc.GhcRn
defaultBindExpr = Ghc.HsVar Ghc.noExtField (Ghc.noLocA Ghc.bindMName)

defaultBindStmtX :: Ghc.XBindStmtRn
defaultBindStmtX =
  Ghc.XBindStmtRn
    { Ghc.xbsrn_bindOp = Ghc.SyntaxExprRn defaultBindExpr
    , Ghc.xbsrn_failOp = Nothing
    }

transformStmt
  :: Env
  -> [StmtTransformer]
  -> Ghc.ExprLStmt Ghc.GhcRn
  -> Ghc.ExprLStmt Ghc.GhcRn
transformStmt env stmtTransformers (Ghc.L loc stmt) = Ghc.L loc $ case stmt of
  Ghc.BodyStmt Ghc.NoExtField (Ghc.L bL body) thenExpr Ghc.NoSyntaxExprRn ->
    case transformBodyStmt body of
      Body b -> Ghc.BodyStmt Ghc.noExtField (Ghc.L bL b) thenExpr Ghc.noSyntaxExpr
      Bind pat b -> Ghc.BindStmt defaultBindStmtX pat (Ghc.L bL b)
  Ghc.BindStmt x pat (Ghc.L bL body) ->
    case transformBindStmt pat body of
      Body b -> Ghc.BodyStmt Ghc.noExtField (Ghc.L bL b) (Ghc.SyntaxExprRn defaultThenExpr) Ghc.noSyntaxExpr
      Bind p b -> Ghc.BindStmt x p (Ghc.L bL b)
  Ghc.LastStmt Ghc.NoExtField (Ghc.L bL body) Nothing Ghc.NoSyntaxExprRn ->
    case transformBodyStmt body of
      Body b -> Ghc.LastStmt Ghc.noExtField (Ghc.L bL b) Nothing Ghc.noSyntaxExpr
      Bind pat b -> Ghc.BindStmt defaultBindStmtX pat (Ghc.L bL b)
  _ -> transform env stmt -- handles let statements and anything else
  where
    transformBodyStmt = \case
      Ghc.HsPar x l (Ghc.L bL inner) r ->
        case transformBodyStmt inner of
          Body b -> Body $ Ghc.HsPar x l (Ghc.L bL b) r
          s -> s
      body | isAppOf loopNames body ->
              Body $ transformExpr env (transformLoop : stmtTransformers) body
           | isAppOf [whenName env] body ->
              Body $ transformExpr env stmtTransformers body
      Ghc.HsIf ix predi t e ->
        Body (Ghc.HsIf ix predi
                (transformExpr env stmtTransformers <$> t)
                (transformExpr env stmtTransformers <$> e)
             )
      Ghc.HsCase cx scrut mg ->
        Body (Ghc.HsCase cx (transform env scrut)
               $ transformMatchGroup env stmtTransformers mg)
      body -> applyTransformers env stmtTransformers (Body $ transform env body)

    transformBindStmt lPat body = applyTransformers env stmtTransformers (Bind lPat body)

    loopNames = getLoopNames env

transformMatchGroup :: Env -> [StmtTransformer] -> Ghc.MatchGroup Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn) -> Ghc.MatchGroup Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
transformMatchGroup env stmtTransformers mg = mg { Ghc.mg_alts = map (fmap transformMatch) <$> Ghc.mg_alts mg }
  where
    transformMatch :: Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn) -> Ghc.Match Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
    transformMatch match =
      match { Ghc.m_pats = transform env <$> Ghc.m_pats match
            , Ghc.m_grhss = (Ghc.m_grhss match)
              { Ghc.grhssGRHSs =
                  map (fmap transformGRHS) (Ghc.grhssGRHSs (Ghc.m_grhss match))
              }
            }

    transformGRHS :: Ghc.GRHS Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn) -> Ghc.GRHS Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
    transformGRHS (Ghc.GRHS x guards body) =
      Ghc.GRHS x
        (transform env <$> guards)
        (transformExpr env stmtTransformers <$> body)

transformExpr :: Env -> [StmtTransformer] -> Ghc.HsExpr Ghc.GhcRn -> Ghc.HsExpr Ghc.GhcRn
transformExpr env stmtTransformers = \case
  Ghc.HsLam lX mg ->
    Ghc.HsLam lX (transformMatchGroup env stmtTransformers mg)
  Ghc.HsPar x a inner b ->
    Ghc.HsPar x a (transformExpr env stmtTransformers <$> inner) b
  Ghc.OpApp x lExpr oExpr rExpr -> -- TODO check lExpr
    Ghc.OpApp x (transform env lExpr) oExpr $ transformExpr env stmtTransformers <$> rExpr
  s@(Ghc.HsApp x fExpr aExpr) | isAppOf (whenName env : loopNames) s ->
--     Ghc.HsApp x (transform env fExpr) (transformExpr env (transformLoop : stmtTransformers) <$> aExpr)
--   s@(Ghc.HsApp x fExpr aExpr) | isAppOf [whenName env] s ->
    Ghc.HsApp x (transform env fExpr) (transformExpr env stmtTransformers <$> aExpr)
  Ghc.HsDo m (Ghc.DoExpr Nothing) stmts ->
    let newStmts = map (transformStmt env stmtTransformers) <$> stmts
     in Ghc.HsDo m (Ghc.DoExpr Nothing) newStmts
  expr -> case applyTransformers env stmtTransformers (Body $ transform env expr) of
            Body b -> b
            Bind _ b -> b -- problematic...

  where
    loopNames = getLoopNames env

applyTransformers :: Env -> [StmtTransformer] -> Stmt -> Stmt
applyTransformers env stmtTransformers st =
  foldr (\t s -> t env s) st stmtTransformers

getLoopNames :: Env -> [Ghc.Name]
getLoopNames env =
  [ forLoopName env
  , repeatLName env
--  , whenName env
--  , whileLoopName env
  ]

isTargetStmt
  :: Env
  -> Ghc.StmtLR Ghc.GhcRn Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> Bool
isTargetStmt env = \case
  Ghc.LastStmt _ body _ _ -> isTargetExpr (Ghc.unLoc body)
  Ghc.BodyStmt _ body _ _ -> isTargetExpr (Ghc.unLoc body)
  _ -> False
  where
    loopNames = whenName env : getLoopNames env
    isTargetExpr expr =
      -- look for application of earlyReturn or forLoop with a do block argument.
      -- This might involve looking through applications of $ and parens.
      isAppOf [earlyReturnName env] expr
        -- Lump loops in with early return for now
      || isAppOf loopNames expr
      || isIfThenElseWithEarlyReturn expr
      || isCaseWithTarget expr
    isIfThenElseWithEarlyReturn = \case
      Ghc.HsIf _ _pred (Ghc.L _ t) (Ghc.L _ e) ->
        exprContainsTarget t || isTargetExpr t
        || exprContainsTarget e || isTargetExpr e
      _ -> False
    exprContainsTarget = \case
      Ghc.HsPar _ _ inner _ -> exprContainsTarget $ Ghc.unLoc inner
      Ghc.HsApp _ expr _ | isAppOf [earlyReturnName env] $ Ghc.unLoc expr -> True
      Ghc.HsApp _ fExpr arg -> isAppOf loopNames (Ghc.unLoc fExpr)
                            && exprContainsTarget (Ghc.unLoc arg)
      Ghc.OpApp _ (Ghc.L _ leftExpr)
                  (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName)))
                  (Ghc.L _ rightExpr)
        -> isAppOf loopNames leftExpr
           && fName == Ghc.dollarName
           && exprContainsTarget rightExpr
      Ghc.HsDo _ (Ghc.DoExpr Nothing) (Ghc.L _ stmts)
        -> any (isTargetStmt env . Ghc.unLoc) stmts
      _ -> False
    isCaseWithTarget = \case
      Ghc.HsCase _ _ mg -> any (matchWithTarget . Ghc.unLoc) (Ghc.unLoc $ Ghc.mg_alts mg)
      _ -> False
    matchWithTarget = any (grhsWithTarget . Ghc.unLoc) . Ghc.grhssGRHSs . Ghc.m_grhss
    grhsWithTarget (Ghc.GRHS _ _guards body) = isTargetExpr (Ghc.unLoc body)

isAppOf :: [Ghc.Name] -> Ghc.HsExpr Ghc.GhcRn -> Bool
isAppOf names = \case
  Ghc.HsVar _ (Ghc.L _ name) -> name `elem` names
  Ghc.HsPar _ _ inner _ -> isAppOf names (Ghc.unLoc inner)
  Ghc.OpApp _ (Ghc.L _ leftExpr)
              (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName)))
              _
    -> fName == Ghc.dollarName && isAppOf names leftExpr
        -- TODO handle strict dollar as well
  Ghc.HsApp _ (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName))) _argExpr
    -> fName `elem` names
  Ghc.HsApp _ fExpr _
    -> isAppOf names (Ghc.unLoc fExpr)
  _ -> False

-- isAppOfWithLifts :: Env -> [Ghc.Name] -> Ghc.HsExpr Ghc.GhcRn -> Bool
-- isAppOfWithLifts env names = \case
--   Ghc.HsApp _ (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName))) argExpr
--     | fName == liftName env -> isAppOfWithLifts env names (Ghc.unLoc argExpr)
--   expr -> isAppOf names expr

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
