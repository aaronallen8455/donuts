{-# LANGUAGE OverloadedStrings #-}
module Donuts.Internal.Plugin.Renamer
  ( renamedResultAction
  ) where

import           Data.Data (Data)
import qualified Data.Data as Data
import           Data.Maybe

import qualified Donuts.Internal.GhcFacade as Ghc

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
  , evalMutVarStateName :: Ghc.Name
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
  Ghc.Found _ apiMod <-
    Ghc.runTcPluginM $
      Ghc.findImportedModule (Ghc.mkModuleName "Donuts.Api") Ghc.NoPkgQual
  Ghc.Found _ internalApiMod <-
    Ghc.runTcPluginM $
      Ghc.findImportedModule (Ghc.mkModuleName "Donuts.Internal.Api") Ghc.NoPkgQual

  env <- Ghc.runTcPluginM $ MkEnv
    <$> Ghc.lookupOrig apiMod (Ghc.mkVarOcc "earlyReturn")
    <*> Ghc.lookupOrig internalApiMod (Ghc.mkVarOcc "earlyReturnWrapDo")
    <*> Ghc.lookupOrig apiMod (Ghc.mkVarOcc "forL")
    <*> Ghc.lookupOrig apiMod (Ghc.mkVarOcc "whileL")
    <*> Ghc.lookupOrig apiMod (Ghc.mkVarOcc "repeatL")
    <*> Ghc.lookupOrig apiMod (Ghc.mkVarOcc "continueL")
    <*> Ghc.lookupOrig apiMod (Ghc.mkVarOcc "breakL")
    <*> Ghc.lookupOrig internalApiMod (Ghc.mkVarOcc "lift")
    <*> Ghc.lookupOrig apiMod (Ghc.mkVarOcc "when")
    <*> Ghc.lookupOrig apiMod (Ghc.mkDataOcc ":=")
    <*> Ghc.lookupOrig internalApiMod (Ghc.mkVarOcc "newMutVar")
    <*> Ghc.lookupOrig internalApiMod (Ghc.mkVarOcc "evalMutVarState")
    <*> Ghc.lookupOrig internalApiMod (Ghc.mkVarOcc "setMutVar")
    <*> Ghc.lookupOrig internalApiMod (Ghc.mkVarOcc "getMutVar")
    <*> Ghc.lookupOrig apiMod (Ghc.mkDataOcc "Mut")
    <*> Ghc.lookupOrig internalApiMod (Ghc.mkVarOcc "not")
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
                 ] body
         then body
         else addApp (liftName env) body
  , transformBindBody = addApp . liftName
  , bindVars = fmap . fmap . addApp . liftName
  }

transformMutVar :: Ghc.Name -> StmtTransformer
transformMutVar varName = MkStmtTransformer
  { transformBody = \env -> \case
      expr@(Ghc.OpApp _ (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ lName)))
                             (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ oName)))
                             rExpr)
        | oName == mutVarAssignOpName env
        , lName == varName
        -> -- asignment of this variable
          Ghc.HsApp Ghc.noComments
                    (Ghc.nlHsVar $ setMutVarName env)
                    rExpr
        | oName == mutVarAssignOpName env
        -> -- assignment for outer var.
           expr
      expr
        | isAppOf [breakLName env, continueLName env, newMutVarName env] expr ->
          expr
        | otherwise ->
          addApp (liftName env) expr
  , transformBindBody = addApp . liftName
  , bindVars = \env bnds ->
      (varName, Ghc.nl_HsVar $ getMutVarName env)
       : (fmap (addApp $ liftName env) <$> bnds)
  }

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
transformStmt env stmtTransformers stmt = addVarBinds $ case stmt of
  Ghc.BodyStmt Ghc.NoExtField (Ghc.L bL body) _thenExpr Ghc.NoSyntaxExprRn ->
    Result
      [ Ghc.BodyStmt Ghc.noExtField
                     (Ghc.L bL (transformBodyStmt body))
                     (Ghc.SyntaxExprRn defaultThenExpr)
                     Ghc.noSyntaxExpr]

  Ghc.BindStmt _x pat (Ghc.L bL body)
    | Just varName <- mutVarDeclPat env (Ghc.unLoc pat)
    , let newBody = transformExpr env stmtTransformers body
    -> OpenDo (transformMutVar varName : stmtTransformers)
         ( \stmts ->
           let b = Ghc.noLocA
                 . Ghc.OpApp Ghc.defaultFixity
                     (Ghc.L bL newBody)
                     (Ghc.noLocA defaultBindExpr)
                 . Ghc.noLocA
                 $ addApp (evalMutVarStateName env)
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
          (Ghc.PatBind _
            (Ghc.L _ pat)
            Ghc.GRHSs
              { Ghc.grhssGRHSs = [Ghc.L _ (Ghc.GRHS _ [] (Ghc.L valL val))] }
          )
        ] <- Ghc.bagToList binds
    , Just varName <- mutVarDeclPat env pat
    -> OpenDo (transformMutVar varName : stmtTransformers)
         ( \stmts ->
           let newVarDo = applyBodyTransformers env stmtTransformers
                 $ Ghc.HsApp Ghc.noComments
                     (Ghc.L valL $ addApp (newMutVarName env) $ transform env val)
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
      body | isAppOf loopNames body ->
              transformExpr env (transformLoop : stmtTransformers) body
           | isAppOf [whenName env] body ->
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

mutVarDeclPat :: Env -> Ghc.Pat Ghc.GhcRn -> Maybe Ghc.Name
mutVarDeclPat env = \case
  Ghc.ConPat _
   (Ghc.L _ conName)
   (Ghc.PrefixCon [] [Ghc.L _ (Ghc.VarPat _ (Ghc.L _ varName))])
     | conName == mutConName env -> Just varName
  _ -> Nothing

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
  Ghc.OpApp x lExpr oExpr@(Ghc.L _ (Ghc.HsVar _ (Ghc.L _ fName))) rExpr
    | fName == Ghc.dollarName
    -> Ghc.OpApp x (transform env lExpr) oExpr
         $ transformExpr env stmtTransformers <$> rExpr
  s@(Ghc.HsApp x fExpr aExpr) | isAppOf (whenName env : loopNames) s ->
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

applyBodyTransformers :: Env -> [StmtTransformer] -> Ghc.HsExpr Ghc.GhcRn -> Ghc.HsExpr Ghc.GhcRn
applyBodyTransformers env stmtTransformers st =
  foldr (`transformBody` env) st stmtTransformers

applyBindTransformers :: Env -> [StmtTransformer] -> Ghc.HsExpr Ghc.GhcRn -> Ghc.HsExpr Ghc.GhcRn
applyBindTransformers env stmtTransformers st =
  foldr (`transformBindBody` env) st stmtTransformers

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
  Ghc.BindStmt _ pat _ -> isJust $ mutVarDeclPat env (Ghc.unLoc pat)
  Ghc.LetStmt _ (Ghc.HsValBinds _ (Ghc.XValBindsLR (Ghc.NValBinds [(_, binds)] _)))
    | Ghc.isSingletonBag binds
      , [Ghc.L _ (Ghc.PatBind _ (Ghc.L _ pat) _)] <- Ghc.bagToList binds
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
