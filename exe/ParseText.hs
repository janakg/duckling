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
import Asterius.Types
import Asterius.Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Duckling.Core
import Duckling.Data.TimeZone
import Duckling.Resolve (DucklingTime)
import qualified Data.Time as Time
import Data.Fixed (Pico)
import Duckling.Testing.Types
foreign export javascript "parseText" parseHandler :: JSString -> JSString

main ::  IO ()
main = do
    -- tzs <- loadTimeZoneSeries "/usr/share/zoneinfo/"
    let parsedResult =  parseHandler "today at 9am"
    print parsedResult

-- type Datetime = (Integer, Int, Int, Int, Int, Pico)

-- dt :: Datetime -> Time.UTCTime
-- dt (year, month, days, hours, minutes, seconds) = Time.UTCTime day diffTime
--   where
--     day = Time.fromGregorian year month days
--     diffTime = Time.timeOfDayToTime $ Time.TimeOfDay hours minutes seconds

-- zTime :: Datetime -> Int -> Time.ZonedTime
-- zTime datetime offset = fromUTC (dt datetime) $ Time.hoursToTimeZone offset

-- refTime :: Datetime -> Int -> DucklingTime
-- refTime datetime offset = fromZonedTime $ zTime datetime offset

-- -- | Parse some text into the given dimensions
-- parseHandler :: ByteString -> IO ()
-- -> HashMap Text TimeZoneSeries

parseHandler :: JSString -> JSString
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
    let inText = textFromJSString givenText
    let parsedResult = parse inText cont opt dims
    textToJSString $Text.decodeUtf8 $ LBS.toStrict  $ encode parsedResult


-- | Parse some text into the given dimensions
-- parseHandler :: ByteString -> HashMap Text TimeZoneSeries -> IO ()
-- parseHandler givent tzs = do
--     let tmp = ""
--     let t = givent
--     let l = "":: B.ByteString
--     let ds = fromMaybe []
--     let tz = "America/Los_Angeles":: B.ByteString 
--     let loc = "en_GB":: B.ByteString
--     let ref = "":: B.ByteString
--     let latent = "False":: B.ByteString
--     print givent
--     -- case t of
--     --     Nothing -> do
--     --         putStrLn "eroro"
--     --     Just tx -> do
--     let timezone = parseTimeZone tz
--     now <- liftIO $ currentReftime tzs timezone
--     let
--         context = Context
--             { referenceTime = maybe now (parseRefTime timezone) ref
--             , locale = maybe (makeLocale (parseLang l) Nothing) parseLocale loc
--             }
--         options = Options {withLatent = parseLatent latent}

--         dimParse = fromMaybe [] $ decode $ LBS.fromStrict $ ""
--         dims = mapMaybe parseDimension dimParse

--         parsedResult = parse (Text.decodeUtf8 t) context options dims
--     print "Done!"
--         --    $ encode parsedResult
--     where
--         defaultLang = EN
--         defaultLocale = makeLocale defaultLang Nothing
--         defaultTimeZone = "America/Los_Angeles"
--         defaultLatent = False

--         parseDimension :: Text -> Maybe (Some Dimension)
--         parseDimension x = fromName x <|> fromCustomName x
--             where
--                 fromCustomName :: Text -> Maybe (Some Dimension)
--                 fromCustomName name = HashMap.lookup name m
--                 m = HashMap.fromList
--                     [ -- ("my-dimension", This (CustomDimension MyDimension))
--                     ]

--         parseTimeZone :: ByteString -> Text
--         parseTimeZone = maybe defaultTimeZone Text.decodeUtf8

--         parseLocale :: ByteString -> Locale
--         parseLocale x = maybe defaultLocale (`makeLocale` mregion) mlang
--             where
--                 (mlang, mregion) = case chunks of
--                     [a, b] -> (readMaybe a :: Maybe Lang, readMaybe b :: Maybe Region)
--                     _      -> (Nothing, Nothing)
--                 chunks = map Text.unpack . Text.split (== '_') . Text.toUpper
--                     $ Text.decodeUtf8 x

--         parseLang :: ByteString -> Lang
--         parseLang l = fromMaybe defaultLang $ l >>=
--             readMaybe . Text.unpack . Text.toUpper . Text.decodeUtf8

--         parseRefTime :: Text -> ByteString -> DucklingTime
--         parseRefTime timezone refTime = makeReftime tzs timezone utcTime
--             where
--                 msec = read $ Text.unpack $ Text.decodeUtf8 refTime
--                 utcTime = posixSecondsToUTCTime $ fromInteger msec / 1000

--         parseLatent :: ByteString -> Bool
--         parseLatent x = fromMaybe defaultLatent
--             (readMaybe (Text.unpack $ Text.toTitle $ Text.decodeUtf8 $ fromMaybe empty x)::Maybe Bool)
