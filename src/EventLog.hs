{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, BlockArguments #-}

module EventLog where

import RIO
import Data.Text
import Data.Time


data EventLog = EventLog
    { utcTime :: UTCTime
    , logLevel :: LogLevel
    , logMessage :: Text
    } deriving (Generic)

