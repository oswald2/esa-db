
module App where

import           RIO

data Logging a = Logging
    { appLogFunc :: LogFunc
    , applicationCtx :: a
    }
