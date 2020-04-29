{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-full-laziness #-}

module Main (main) where

import Control.Monad
import Data.Some
import System.Environment

import Duckling.Debug
import Duckling.Dimensions.Types
import Duckling.Locale

main :: IO ()
main = do
    void $ do
        debug en "My number is 123" []
        debug en "Wednesday 5:00PM 3/29/2017" []
        where
            en = makeLocale EN Nothing