{-# LANGUAGE ScopedTypeVariables #-}

module App where

import Data.Time
import Database.Selda
import Database.Selda.Backend
import Database.Selda.SQLite
import Db
import RIO

import AppEnv
import EventLog

runIO :: RIO (AppEnv () (SeldaConnection SQLite)) () -> IO ()
runIO app =
    withTermLogger $ \termLogger ->
        withDb "test.db" $ \dbRef ->
            withDbLogger dbRef $ \dbLogger -> do
                let loggers = mappend termLogger dbLogger
                let appEnv = AppEnv loggers dbRef ()
                runRIO appEnv app

withTermLogger :: (LogFunc -> IO ()) -> IO ()
withTermLogger a = bracket setupTermLogger snd (a . fst)

withDb :: FilePath -> (IORef (SeldaConnection SQLite) -> IO ()) -> IO ()
withDb fp a = bracket (setupDb fp) snd (a . fst)

withDbLogger :: IORef (SeldaConnection SQLite) -> (LogFunc -> IO ())-> IO ()
withDbLogger dbRef a =
    let dbLogger = setupDbLogger dbRef
    in a dbLogger

setupTermLogger :: IO (LogFunc, IO ())
setupTermLogger = do
    logOptions <- logOptionsHandle stderr True
    newLogFunc logOptions

setupDb :: FilePath -> IO (IORef (SeldaConnection SQLite), IO ())
setupDb fp = do
    dbRef <- sqliteOpen fp >>= newIORef
    let killDb = readIORef dbRef >>= seldaClose
    tryCreateEventLogTable dbRef
    pure (dbRef, killDb)

setupDbLogger :: IORef (SeldaConnection SQLite) -> LogFunc
setupDbLogger dbRef =
    mkLogFunc $ \_ _ ll t -> do
            time <- getCurrentTime
            insertEventLog dbRef $ EventLog time ll $ textDisplay t
