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
runIO app = do
    (termLogger, killTermLogger) <- setupTermLogger
    (dbRef, killDb) <- setupDb "test.db"
    let dbLogger = setupDbLogger dbRef

    let loggers = mappend termLogger dbLogger
    let appEnv = AppEnv loggers dbRef ()
    runRIO appEnv app

    killDb
    killTermLogger

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
