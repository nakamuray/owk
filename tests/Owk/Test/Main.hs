{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Owk.Test.Main (tests) where

import Test.Framework.TH
import Test.HUnit

import Data.Conduit

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Data.Conduit.Owk
import Owk.Type as Type
import Owk.Util
import Owk.Test.Util

import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Vector as V

tests = $(testGroupGenerator)


case_sum = do
    ret <- testOwkString script_sum $ map (Number . I ) [1..10]
    ret @?= [[Number (I 55)]]
  where
    script_sum = [s|
        sum = ref 0
        main = {
          sum := sum () + _
        }
        end = {
          print : sum ()
        }
        |]

case_tail = do
    ret <- testOwkString script_tail $ map (Number . I ) [1..10]
    ret @?= map ((:[]) . Number . I) [2..10]
  where
    script_tail = [s|
        # read and ignore first object
        getobj ()
        main = print
        |]

case_readme_string = do
    ret <- flip testOwkString [] [s|
        s = "hello, owk"
        print "\u3042"

        main = undef
    |]
    ret @?= [[String "あ"]]

case_readme_number = do
    ret <- flip testOwkString [] [s|
        i = 2
        j = 10.1
        print (i * j)

        main = undef
    |]
    ret @?= [[Number (D 20.2)]]

case_readme_bool = do
    ret <- flip testOwkString [] [s|
        t = true
        f = false
        print t, f

        main = undef
    |]
    ret @?= [[Bool True, Bool False]]

case_readme_dict = do
    ret <- flip testOwkString [] [s|
        d = { key => "value", key2 => 100 }
        print d.key # => value
        print (d ["key"]) # => value
        d2 = d { key2 => 200, key3 => "spam" }
        d3 = d2 ["key4", "egg"]
        print d2.key2, d3.key4

        main = undef
    |]
    ret @?= [[String "value"], [String "value"], [Number (I 200), String "egg"]]

case_readme_list = do
    ret <- flip testOwkString [] [s|
        L = [1, 2, 3, "4"]
        print (L[0]) # => 1
        L2 = L[1, 2]
        print L2 # => [2, 3]

        main = undef
    |]
    ret @?= [[Number (I 1)], [List (V.fromList [Number (I 2), Number (I 3)])]]

case_readme_function = do
    ret <- flip testOwkString [] [s|
        f = { print "hi" }
        f () # => hi

        f2 = name -> { print "hi,", name }
        f2 "nakamuray" # => hi, nakamuray

        f3 = i -> { i * 2 }
        print (f3 10) # => 20

        f4 = { _ * 2 }
        print (f4 10) # => 20

        main = undef
    |]
    ret @?= [[String "hi"], [String "hi,", String "nakamuray"], [Number (I 20)], [Number (I 20)]]

case_readme_ref = do
    ret <- flip testOwkString [] [s|
        r = ref 0
        print (r ()) # => 0
        r := 1
        print (r ()) # => 1

        main = undef
    |]
    ret @?= [[Number (I 0)], [Number (I 1)]]

case_readme_operator_app = do
    ret <- flip testOwkString [] [s|
        print : 1 + 1 # => 2
        print (1 + 1) # => 2

        main = undef
    |]
    ret @?= [[Number (I 2)], [Number (I 2)]]

case_readme_operator_if = do
    ret <- flip testOwkString [] [s|
        true ? { print "hi" } # => hi
        false ? { print "hi?" }

        main = undef
    |]
    ret @?= [[String "hi"]]


testOwkString :: T.Text -> [Object] -> IO [[Object]]
testOwkString script inputs = CL.sourceList inputs $= owkString script $$ CL.consume
