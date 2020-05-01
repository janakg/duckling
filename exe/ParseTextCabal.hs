-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative hiding (empty)
import Control.Arrow ((***))
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString, empty)
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Data.Time.LocalTime.TimeZone.Series
import Prelude
import System.Directory
import TextShow
import Text.Read (readMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Duckling.Core
import Duckling.Data.TimeZone
import Duckling.Resolve (DucklingTime)
import qualified Data.Time as Time
import Data.Fixed (Pico)
import Duckling.Testing.Types
-- foreign export javascript "parseText" parseHandler :: JSString -> JSString

main ::  IO ()
main = do
    -- tzs <- loadTimeZoneSeries "/usr/share/zoneinfo/"
    let parsedResult =  parseHandler "today at 9am"
    print parsedResult


parseHandler :: Text -> Text
parseHandler givenText = do
    -- now <- liftIO $ currentReftime tzs (parseTimeZone "")
    let cont = Context
                { locale = makeLocale EN Nothing
                , referenceTime = refTime (2020, 4, 28, 4, 30, 0) (-2)
                    -- 
                }
    let opt = Options
                { withLatent = False
                }
    let dims = []
    let parsedResult = parse givenText cont opt dims
    encode parsedResult
