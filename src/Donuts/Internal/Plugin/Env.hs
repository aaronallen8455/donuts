{-# LANGUAGE OverloadedStrings #-}
module Donuts.Internal.Plugin.Env
  ( Env(..)
  , initEnv
  , getLoopNames
  , getEvalMutVarStateName
  , getNewMutVarName
  ) where

import qualified GHC.LanguageExtensions as LangExt

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
  , newMutVarStrictName :: Ghc.Name
  , evalMutVarStateName :: Ghc.Name
  , evalMutVarStateStrictName :: Ghc.Name
  , setMutVarName :: Ghc.Name
  , setMutVarStrictName :: Ghc.Name
  , getMutVarName :: Ghc.Name
  , mutConName :: Ghc.Name
  , notName :: Ghc.Name
  , strictOn :: Bool -- True => XStrict on
  }

initEnv :: Ghc.TcM Env
initEnv = do
  Ghc.Found _ apiMod <-
    Ghc.runTcPluginM $
      Ghc.findImportedModule (Ghc.mkModuleName "Donuts.Api") Ghc.NoPkgQual
  Ghc.Found _ internalApiMod <-
    Ghc.runTcPluginM $
      Ghc.findImportedModule (Ghc.mkModuleName "Donuts.Internal.Api") Ghc.NoPkgQual
  dflags <- Ghc.getDynFlags

  Ghc.runTcPluginM $ MkEnv
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
    <*> Ghc.lookupOrig internalApiMod (Ghc.mkVarOcc "newMutVarStrict")
    <*> Ghc.lookupOrig internalApiMod (Ghc.mkVarOcc "evalMutVarState")
    <*> Ghc.lookupOrig internalApiMod (Ghc.mkVarOcc "evalMutVarStateStrict")
    <*> Ghc.lookupOrig internalApiMod (Ghc.mkVarOcc "setMutVar")
    <*> Ghc.lookupOrig internalApiMod (Ghc.mkVarOcc "setMutVarStrict")
    <*> Ghc.lookupOrig internalApiMod (Ghc.mkVarOcc "getMutVar")
    <*> Ghc.lookupOrig apiMod (Ghc.mkDataOcc "Mut")
    <*> Ghc.lookupOrig internalApiMod (Ghc.mkVarOcc "not")
    <*> pure (Ghc.xopt LangExt.Strict dflags)

getLoopNames :: Env -> [Ghc.Name]
getLoopNames env =
  [ forLName env
  , repeatLName env
  , whileLName env
  ]

getEvalMutVarStateName :: Bool -> Env -> Ghc.Name
getEvalMutVarStateName isStrict env =
  if isStrict
     then evalMutVarStateStrictName env
     else evalMutVarStateName env

getNewMutVarName :: Bool -> Env -> Ghc.Name
getNewMutVarName isStrict env =
  if isStrict
     then newMutVarStrictName env
     else newMutVarName env
