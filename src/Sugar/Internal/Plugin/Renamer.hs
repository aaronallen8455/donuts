{-# LANGUAGE OverloadedStrings #-}
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
  , forLName :: Ghc.Name
  , whileLName :: Ghc.Name
  , repeatLName :: Ghc.Name
  , continueLName :: Ghc.Name
  , breakLName :: Ghc.Name
  , liftName :: Ghc.Name
  , whenName :: Ghc.Name
  , mutVarAssignOpName :: Ghc.Name
  , newMutVarName :: Ghc.Name
  , setMutVarName :: Ghc.Name
  , getMutVarName :: Ghc.Name
  , mutConName :: Ghc.Name
  , notName :: Ghc.Name
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
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "forL")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "whileL")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "repeatL")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "continueL")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "breakL")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "lift")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "whenL")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "=:")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "newMutVar")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "setMutVar")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "getMutVar")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkDataOcc "Mut")
    <*> Ghc.lookupOrig sugarMod (Ghc.mkVarOcc "not")
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
           let newStmts = Ghc.L loc $
                 transformStmts env [transformEarlyReturn] stmts
            in Just $
              Ghc.HsApp Ghc.noComments
                (Ghc.noLocA (Ghc.HsVar Ghc.noExtField
                  (Ghc.noLocA $ earlyReturnWrapDoName env)))
              (Ghc.noLocA $ Ghc.HsDo m (Ghc.DoExpr Nothing) newStmts)
      _ -> Nothing

data Stmt
  = Body (Ghc.HsExpr Ghc.GhcRn)
         [(Ghc.Name, Ghc.HsExpr Ghc.GhcRn)]
  | Bind (Ghc.LPat Ghc.GhcRn) (Ghc.HsExpr Ghc.GhcRn)
         [(Ghc.Name, Ghc.HsExpr Ghc.GhcRn)]

extractStmtExpr :: Stmt -> Ghc.HsExpr Ghc.GhcRn
extractStmtExpr = \case
  Body x _ -> x
  Bind _ x _ -> x

mapStmtExpr :: (Ghc.HsExpr Ghc.GhcRn -> Ghc.HsExpr Ghc.GhcRn) -> Stmt -> Stmt
mapStmtExpr f = \case
  Body x b -> Body (f x) b
  Bind p x b -> Bind p (f x) b

type StmtTransformer = Env -> Stmt -> Stmt

transformEarlyReturn :: StmtTransformer
transformEarlyReturn env = \case
  Body body bnds -> Body (transformBodyStmt body) bnds
  Bind pat body bnds -> Bind pat (addApp (liftName env) body) bnds
  where
    transformBodyStmt = \case
      body | isAppOf [ earlyReturnName env
                     , breakLName env
                     , continueLName env
                     , mutVarAssignOpName env
                     , newMutVarName env
                     ] body -> body
      body -> addApp (liftName env) body

transformLoop :: StmtTransformer
transformLoop env = \case
  Body body bnds
    | isAppOf [breakLName env, continueLName env, newMutVarName env] body ->
        Body body (fmap (addApp (liftName env)) <$> bnds)
    | otherwise -> Body (addApp (liftName env) body)
                        (fmap (addApp (liftName env)) <$> bnds)
  Bind pat body bnds ->
    Bind pat (addApp (liftName env) body)
      (fmap (addApp (liftName env)) <$> bnds)

transformMutVar :: Ghc.Name -> StmtTransformer
transformMutVar varName env = \case
  Body expr@(Ghc.OpApp _ (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ lName)))
                         (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ oName)))
                         rExpr)
       bnds
    | oName == mutVarAssignOpName env
    , lName == varName
    -> -- asignment of this variable
      Body ( Ghc.HsApp Ghc.noComments
               (Ghc.nlHsVar $ setMutVarName env)
               rExpr
           )
           ((varName, Ghc.nl_HsVar $ getMutVarName env)
                   : (fmap (addApp (liftName env)) <$> bnds))
    | oName == mutVarAssignOpName env
    -> -- assignment for outer var.
       Body expr ((varName, Ghc.nl_HsVar $ getMutVarName env)
                  : (fmap (addApp (liftName env)) <$> bnds))
  Body expr bnds
    | isAppOf [breakLName env, continueLName env, newMutVarName env] expr ->
      Body expr
           ((varName, Ghc.nl_HsVar $ getMutVarName env)
            : (fmap (addApp $ liftName env) <$> bnds))
    | otherwise ->
      Body (addApp (liftName env) expr)
           ((varName, Ghc.nl_HsVar $ getMutVarName env)
            : (fmap (addApp $ liftName env) <$> bnds))
  Bind pat body bnds ->
    Bind pat (addApp (liftName env) body)
         ((varName, Ghc.nl_HsVar $ getMutVarName env)
          : (fmap (addApp $ liftName env) <$> bnds))

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
transformStmt env stmtTransformers = \case
  -- TODO add mutVar binds to every statement no need for per case?
  Ghc.BodyStmt Ghc.NoExtField (Ghc.L bL body) _thenExpr Ghc.NoSyntaxExprRn ->
    Result $
    case transformBodyStmt body of
      Body b bnds -> bodyToStmts bL b bnds
      Bind pat b bnds -> bindToStmts bL pat b bnds

  -- TODO Mut bind
  Ghc.BindStmt _x pat (Ghc.L bL body) -> Result $
    case transformBindStmt pat body of
      Body b bnds -> bodyToStmts bL b bnds
      Bind p b bnds -> bindToStmts bL p b bnds

  Ghc.LastStmt Ghc.NoExtField (Ghc.L bL body) Nothing Ghc.NoSyntaxExprRn ->
    Result $
    case transformBodyStmt body of
      Body b bnds ->
        (mkMutVarBindStmt <$> bnds)
        ++ [Ghc.LastStmt Ghc.noExtField (Ghc.L bL b) Nothing Ghc.noSyntaxExpr]
      Bind pat b bnds -> bindToStmts bL pat b bnds

  Ghc.LetStmt _ (Ghc.HsValBinds _ (Ghc.XValBindsLR (Ghc.NValBinds [(_, binds)] _)))
    | Ghc.isSingletonBag binds
      , [Ghc.L loc
          (Ghc.PatBind _
            (Ghc.L _
              (Ghc.ConPat _
                (Ghc.L _ conName)
                (Ghc.PrefixCon [] [Ghc.L _ (Ghc.VarPat _ (Ghc.L _ varName))])
              )
            )
            Ghc.GRHSs
              { Ghc.grhssGRHSs = [Ghc.L _ (Ghc.GRHS _ [] (Ghc.L valL val))] }
          )
        ] <- Ghc.bagToList binds
    , conName == mutConName env
    -> OpenDo (transformMutVar varName : stmtTransformers)
         ( \stmts ->
           let newVarDo = applyTransformers env stmtTransformers
                 $ Body
                     ( Ghc.HsApp Ghc.noComments
                        (Ghc.L valL $ addApp (newMutVarName env) $ transform env val)
                        (Ghc.noLocA $ Ghc.HsDo Ghc.noExtField (Ghc.DoExpr Nothing) $ Ghc.noLocA stmts)
                     )
                     []
            in case newVarDo of
                  Body b bnds ->
                    (mkMutVarBindStmt <$> bnds)
                    ++ [Ghc.LastStmt Ghc.noExtField (Ghc.L loc b) Nothing Ghc.noSyntaxExpr]
                  Bind pat b bnds -> bindToStmts loc pat b bnds
         )

  stmt -> Result $ (mkMutVarBindStmt <$> mutVarBinds)
            ++ [transform env stmt] -- handles let statements and anything else
  where
    bodyToStmts bL b bnds =
      (mkMutVarBindStmt <$> bnds) ++
        [ Ghc.BodyStmt Ghc.noExtField
                       (Ghc.L bL b)
                       (Ghc.SyntaxExprRn defaultThenExpr)
                       Ghc.noSyntaxExpr]

    bindToStmts bL pat b bnds =
      (mkMutVarBindStmt <$> bnds) ++
        [ Ghc.BindStmt defaultBindStmtX pat (Ghc.L bL b) ]

    mkMutVarBindStmt (name, expr) =
      Ghc.BindStmt defaultBindStmtX (Ghc.nlVarPat name) (Ghc.noLocA expr)

    transformBodyStmt = \case
      Ghc.HsPar x l (Ghc.L bL inner) r ->
        mapStmtExpr (\b -> Ghc.HsPar x l (Ghc.L bL b) r)
          $ transformBodyStmt inner
      body | isAppOf loopNames body ->
              Body (transformExpr env (transformLoop : stmtTransformers) body)
                   mutVarBinds
           | isAppOf [whenName env] body ->
              Body (transformExpr env stmtTransformers body)
                   mutVarBinds
      Ghc.HsIf ix predi t e ->
        Body (Ghc.HsIf ix predi
                (transformExpr env stmtTransformers <$> t)
                (transformExpr env stmtTransformers <$> e)
             ) mutVarBinds
      Ghc.HsCase cx scrut mg ->
        Body (Ghc.HsCase cx (transform env scrut)
               $ transformMatchGroup env stmtTransformers mg)
             mutVarBinds
      body -> applyTransformers env stmtTransformers (Body (transform env body) [])

    transformBindStmt lPat body =
      applyTransformers env stmtTransformers (Bind lPat body [])

    loopNames = getLoopNames env

    mutVarBinds =
      case applyTransformers env stmtTransformers (Body Ghc.noExpr []) of
        Body _ b -> b
        Bind _ _ b -> b

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
  Ghc.OpApp _ (Ghc.L _ (Ghc.HsApp _ (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName))) predExp))
              (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ oName)))
              rExpr
    | oName == Ghc.dollarName
    , fName == whileLName env ->
        addApp (repeatLName env)
        . transformExpr env stmtTransformers
        . transformWhileBody predExp
        $ Ghc.unLoc rExpr

  -- is this needed?
  Ghc.HsApp _ (Ghc.L _ (Ghc.HsApp _ (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName))) predExp)) argExp
    | fName == whileLName env ->
        addApp (repeatLName env)
        . transformExpr env stmtTransformers
        . transformWhileBody predExp
        $ Ghc.unLoc argExp

  Ghc.HsLam lX mg ->
    Ghc.HsLam lX (transformMatchGroup env stmtTransformers mg)
  Ghc.HsPar x a inner b ->
    Ghc.HsPar x a (transformExpr env stmtTransformers <$> inner) b
  Ghc.OpApp x lExpr oExpr rExpr -> -- TODO check lExpr
    Ghc.OpApp x (transform env lExpr) oExpr
      $ transformExpr env stmtTransformers <$> rExpr
  s@(Ghc.HsApp x fExpr aExpr) | isAppOf (whenName env : loopNames) s ->
    Ghc.HsApp x (transform env fExpr)
      $ transformExpr env stmtTransformers <$> aExpr
  Ghc.HsDo m (Ghc.DoExpr Nothing) stmts ->
    let newStmts = transformStmts env stmtTransformers <$> stmts
     in Ghc.HsDo m (Ghc.DoExpr Nothing) newStmts
  expr -> extractStmtExpr $
    applyTransformers env stmtTransformers (Body (transform env expr) [])

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

applyTransformers :: Env -> [StmtTransformer] -> Stmt -> Stmt
applyTransformers env stmtTransformers st =
  foldr (\t s -> t env s) st stmtTransformers

getLoopNames :: Env -> [Ghc.Name]
getLoopNames env =
  [ forLName env
  , repeatLName env
  , whileLName env
  ]

isTargetStmt
  :: Env
  -> Ghc.StmtLR Ghc.GhcRn Ghc.GhcRn (Ghc.LHsExpr Ghc.GhcRn)
  -> Bool
isTargetStmt env = \case
  Ghc.LastStmt _ body _ _ -> isTargetExpr (Ghc.unLoc body)
  Ghc.BodyStmt _ body _ _ -> isTargetExpr (Ghc.unLoc body)
  Ghc.LetStmt _ (Ghc.HsValBinds _ (Ghc.XValBindsLR (Ghc.NValBinds [(_, binds)] _)))
    | Ghc.isSingletonBag binds
      , [Ghc.L _
          (Ghc.PatBind _
            (Ghc.L _
              (Ghc.ConPat _
                (Ghc.L _ conName)
                _
              )
            ) _
          )
        ] <- Ghc.bagToList binds
    -> conName == mutConName env
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
