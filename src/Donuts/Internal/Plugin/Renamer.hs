module Donuts.Internal.Plugin.Renamer
  ( renamedResultAction
  ) where

import           Data.Data (Data)
import qualified Data.Data as Data
import           Data.Maybe

import qualified Donuts.Internal.GhcFacade as Ghc
import           Donuts.Internal.Plugin.Env (Env(..), getEvalMutVarStateName, getLoopNames, getNewMutVarName, initEnv)
import           Donuts.Internal.Plugin.Util (addApp, isAppOf)
import           Donuts.Internal.Plugin.StmtTransformer

renamedResultAction
  :: Ghc.TcGblEnv
  -> Ghc.HsGroup Ghc.GhcRn
  -> Ghc.TcM (Ghc.TcGblEnv, Ghc.HsGroup Ghc.GhcRn)
renamedResultAction gblEnv group = do
  env <- initEnv
  pure (gblEnv, transform env group)

newtype T a = T (a -> Maybe a)

-- | A generic bottom up traversal that applies transformations to do blocks
-- containing one of the target identifiers.
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
           let newStmts = Ghc.L loc $
                 transformStmts env [transformEarlyReturn] stmts
            in Just $
              Ghc.HsApp Ghc.noComments
                (Ghc.noLocA (Ghc.HsVar Ghc.noExtField
                  (Ghc.noLocA $ earlyReturnWrapDoName env)))
              (Ghc.noLocA $ Ghc.HsDo m (Ghc.DoExpr Nothing) newStmts)
      _ -> Nothing

transformStmts
  :: Env
  -> [StmtTransformer]
  -> [Ghc.ExprLStmt Ghc.GhcRn]
  -> [Ghc.ExprLStmt Ghc.GhcRn]
transformStmts env stmtTransformers = go
  where
  go [] = []
  go (Ghc.L loc stmt : stmts) =
    let r = transformStmt env stmtTransformers stmt
     in case r of
          Result newStmts -> (Ghc.L loc <$> newStmts) ++ go stmts
          OpenDo newTransformers withStmts ->
            Ghc.L loc <$>
              withStmts (transformStmts env newTransformers stmts)

data TransformStmtResult
  = Result [Ghc.ExprStmt Ghc.GhcRn]
  | OpenDo [StmtTransformer]
           ([Ghc.ExprLStmt Ghc.GhcRn] -> [Ghc.ExprStmt Ghc.GhcRn])

transformStmt
  :: Env
  -> [StmtTransformer]
  -> Ghc.ExprStmt Ghc.GhcRn
  -> TransformStmtResult
transformStmt env stmtTransformers stmt = addVarBinds $ case stmt of
  Ghc.BodyStmt Ghc.NoExtField (Ghc.L bL body) _thenExpr Ghc.NoSyntaxExprRn ->
    Result
      [ Ghc.BodyStmt Ghc.noExtField
                     (Ghc.L bL (transformBodyStmt body))
                     (Ghc.SyntaxExprRn defaultThenExpr)
                     Ghc.noSyntaxExpr]

  Ghc.BindStmt _x pat (Ghc.L bL body)
    | Just (varName, isStrict) <- mutVarDeclPat env (Ghc.unLoc pat)
    , let newBody = transformExpr env stmtTransformers body
    -> OpenDo (transformMutVar isStrict varName : stmtTransformers)
         ( \stmts ->
           let b = Ghc.noLocA
                 . Ghc.OpApp Ghc.defaultFixity
                     (Ghc.L bL newBody)
                     (Ghc.noLocA defaultBindExpr)
                 . Ghc.noLocA
                 $ addApp (getEvalMutVarStateName isStrict env)
                     (Ghc.HsDo Ghc.noExtField (Ghc.DoExpr Nothing)
                       $ Ghc.noLocA stmts)
            in mutVarBindStmts
                 ++ [ Ghc.LastStmt Ghc.noExtField b Nothing Ghc.noSyntaxExpr ]
         )

  Ghc.BindStmt _x pat (Ghc.L bL body) -> Result $
    let newBody = applyBindTransformers env stmtTransformers body
     in [ Ghc.BindStmt defaultBindStmtX pat (Ghc.L bL newBody) ]

  Ghc.LastStmt Ghc.NoExtField (Ghc.L bL body) Nothing Ghc.NoSyntaxExprRn ->
    Result
      [Ghc.LastStmt Ghc.noExtField
         (Ghc.L bL (transformBodyStmt body)) Nothing Ghc.noSyntaxExpr]

  Ghc.LetStmt _ (Ghc.HsValBinds _ (Ghc.XValBindsLR (Ghc.NValBinds [(_, binds)] _)))
    | Ghc.isSingletonBag binds
      , [Ghc.L loc
          (Ghc.PatBind
            { Ghc.pat_lhs = (Ghc.L _ pat)
            , Ghc.pat_rhs =
              Ghc.GRHSs
                { Ghc.grhssGRHSs = [Ghc.L _ (Ghc.GRHS _ [] (Ghc.L valL val))] }
            }
          )
        ] <- Ghc.bagToList binds
    , Just (varName, isStrict) <- mutVarDeclPat env pat
    -> OpenDo (transformMutVar isStrict varName : stmtTransformers)
         ( \stmts ->
           let newVarDo = applyBodyTransformers env stmtTransformers
                 $ Ghc.HsApp Ghc.noComments
                     (Ghc.L valL $ addApp (getNewMutVarName isStrict env) $ transform env val)
                     (Ghc.noLocA $ Ghc.HsDo Ghc.noExtField (Ghc.DoExpr Nothing)
                                 $ Ghc.noLocA stmts)
            in mutVarBindStmts
                 ++ [Ghc.LastStmt Ghc.noExtField (Ghc.L loc newVarDo) Nothing Ghc.noSyntaxExpr]
         )

  _ -> Result [transform env stmt] -- handles let statements and anything else

  where
    addVarBinds = \case
      Result stmts -> Result $ mutVarBindStmts ++ stmts
      x -> x

    transformBodyStmt = \case
      Ghc.HsPar x l (Ghc.L bL inner) r ->
        Ghc.HsPar x l (Ghc.L bL $ transformBodyStmt inner) r
      body | isAppOf (whenName env : loopNames) body ->
              transformExpr env stmtTransformers body
      Ghc.HsIf ix predi t e ->
        Ghc.HsIf ix predi
          (transformExpr env stmtTransformers <$> t)
          (transformExpr env stmtTransformers <$> e)
      Ghc.HsCase cx scrut mg ->
        Ghc.HsCase cx (transform env scrut)
          $ transformMatchGroup env stmtTransformers mg
      body -> applyBodyTransformers env stmtTransformers (transform env body)

    loopNames = getLoopNames env

    mkMutVarBindStmt (name, expr) =
      Ghc.BindStmt defaultBindStmtX (Ghc.nlVarPat name) (Ghc.noLocA expr)

    mutVarBindStmts = mkMutVarBindStmt
                  <$> foldr (`bindVars` env) [] stmtTransformers

transformExpr :: Env -> [StmtTransformer] -> Ghc.HsExpr Ghc.GhcRn -> Ghc.HsExpr Ghc.GhcRn
transformExpr env stmtTransformers = \case
  Ghc.OpApp _ (Ghc.L _ (Ghc.HsApp _ (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName))) predExp))
              (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ oName)))
              rExpr
    | oName == Ghc.dollarName
    , fName == whileLName env ->
        addApp (repeatLName env)
        . transformExpr env (transformLoop : stmtTransformers)
        . transformWhileBody predExp
        $ Ghc.unLoc rExpr

  -- is this needed?
  Ghc.HsApp _ (Ghc.L _ (Ghc.HsApp _ (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName))) predExp)) argExp
    | fName == whileLName env ->
        addApp (repeatLName env)
        . transformExpr env (transformLoop : stmtTransformers)
        . transformWhileBody predExp
        $ Ghc.unLoc argExp

  Ghc.HsLam lX mg ->
    Ghc.HsLam lX (transformMatchGroup env stmtTransformers mg)
  Ghc.HsPar x a inner b ->
    Ghc.HsPar x a (transformExpr env stmtTransformers <$> inner) b
  s@(Ghc.OpApp x lExpr oExpr rExpr)
    | isAppOf [whenName env] s
    -> Ghc.OpApp x (transform env lExpr) oExpr
         $ transformExpr env stmtTransformers <$> rExpr
    | isAppOf loopNames s
    -> Ghc.OpApp x (transform env lExpr) oExpr
         $ transformExpr env (transformLoop : stmtTransformers) <$> rExpr
  s@(Ghc.HsApp x fExpr aExpr)
    | isAppOf loopNames s ->
      Ghc.HsApp x (transform env fExpr)
        $ transformExpr env (transformLoop : stmtTransformers) <$> aExpr
    | isAppOf [whenName env] s ->
      Ghc.HsApp x (transform env fExpr)
        $ transformExpr env stmtTransformers <$> aExpr
  Ghc.HsDo m (Ghc.DoExpr Nothing) stmts ->
    let newStmts = transformStmts env stmtTransformers <$> stmts
     in Ghc.HsDo m (Ghc.DoExpr Nothing) newStmts
  expr ->
    applyBodyTransformers env stmtTransformers (transform env expr)

  where
    loopNames = getLoopNames env

    transformWhileBody predExp = \case
      Ghc.HsDo m (Ghc.DoExpr Nothing) stmts ->
        let whenBreak =
              Ghc.HsApp Ghc.noComments
                (addApp (whenName env) . addApp (notName env) <$> predExp)
                (Ghc.noLocA (Ghc.HsVar Ghc.noExtField (Ghc.noLocA $ breakLName env)))
            mkNewStmts ss =
               Ghc.noLocA (Ghc.BodyStmt Ghc.NoExtField
                               (Ghc.noLocA whenBreak)
                               (Ghc.SyntaxExprRn defaultThenExpr)
                               Ghc.noSyntaxExpr)
                : ss
         in Ghc.HsDo m (Ghc.DoExpr Nothing) (mkNewStmts <$> stmts)
      expr -> transformExpr env stmtTransformers expr

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

-- | Extracts the variable name from a mutable variable declaration pattern
mutVarDeclPat :: Env -> Ghc.Pat Ghc.GhcRn -> Maybe (Ghc.Name, Bool)
mutVarDeclPat env = \case
  Ghc.ConPat _
   (Ghc.L _ conName)
   (Ghc.PrefixCon [] [Ghc.L _ pat])
     | conName == mutConName env
     -> case pat of
          Ghc.VarPat _ (Ghc.L _ varName)
            -> Just (varName, strictOn env)
          Ghc.LazyPat _ (Ghc.L _ (Ghc.VarPat _ (Ghc.L _ varName)))
            -> Just (varName, False)
          Ghc.BangPat _ (Ghc.L _ (Ghc.VarPat _ (Ghc.L _ varName)))
            -> Just (varName, True)
          _ -> Nothing
  _ -> Nothing

-- | Returns True if the given statement contains an identifier from the API.
isTargetStmt
  :: Env
  -> Ghc.StmtLR Ghc.GhcRn Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> Bool
isTargetStmt env = \case
  Ghc.LastStmt _ body _ _ -> isTargetExpr (Ghc.unLoc body)
  Ghc.BodyStmt _ body _ _ -> isTargetExpr (Ghc.unLoc body)
  Ghc.BindStmt _ pat _ -> isJust $ mutVarDeclPat env (Ghc.unLoc pat)
  Ghc.LetStmt _ (Ghc.HsValBinds _ (Ghc.XValBindsLR (Ghc.NValBinds [(_, binds)] _)))
    | Ghc.isSingletonBag binds
      , [Ghc.L _ (Ghc.PatBind { Ghc.pat_lhs = Ghc.L _ pat })] <- Ghc.bagToList binds
    -> isJust $ mutVarDeclPat env pat
  _ -> False
  where
    targetNames = whenName env : getLoopNames env
    isTargetExpr expr =
      -- look for application of earlyReturn or forL with a do block argument.
      -- This might involve looking through applications of $ and parens.
      isAppOf [earlyReturnName env] expr
        -- Lump loops in with early return for now
      || isAppOf targetNames expr
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
      Ghc.HsApp _ fExpr arg -> isAppOf targetNames (Ghc.unLoc fExpr)
                            && exprContainsTarget (Ghc.unLoc arg)
      Ghc.OpApp _ (Ghc.L _ leftExpr)
                  (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName)))
                  (Ghc.L _ rightExpr)
        -> isAppOf targetNames leftExpr
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
