{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}

module AppEnv where

import RIO
import Database.Selda
import Database.Selda.Backend
import Database.Selda.SQLite


data AppEnv a b = AppEnv
    { appLogFunc :: LogFunc
    , databaseConnection :: IORef b
    , applicationCtx :: a
    }

instance HasLogFunc (AppEnv a b) where
    logFuncL :: Lens' (AppEnv a b) LogFunc
    logFuncL d (AppEnv x c a) = (\x -> AppEnv x c a) <$> d x

