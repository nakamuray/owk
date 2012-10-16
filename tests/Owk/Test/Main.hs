{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Owk.Test.Main (tests) where

import Test.Framework.TH
import Test.HUnit

import Data.Conduit

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Data.Conduit.Owk
import Owk.Type as Type
import Owk.Util

import qualified Data.Text as T
import qualified Data.Conduit.List as CL

tests = $(testGroupGenerator)


case_sum = do
    ret <- testOwkString script_sum $ map (Number . I ) [1..10]
    ret @?= [[Number (I 55)]]
  where
    script_sum = T.unlines
        [ "sum = ref 0"
        , "main = {"
        , "  sum := sum () + _"
        , "}"
        , "end = {"
        , "  print : sum ()"
        , "}"
        ]


testOwkString :: T.Text -> [Object] -> IO [[Object]]
testOwkString script inputs = CL.sourceList inputs $= owkString script $$ CL.consume
