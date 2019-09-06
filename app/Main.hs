{-# LANGUAGE OverloadedStrings #-}

module Main where

import RIO
import System.Random

import App
import AppEnv

testApp :: RIO (AppEnv () a) ()
testApp = do
    logInfo "Starting 1"
    logWarn "Starting 1"
    logError "Starting 1"

main = runIO testApp
