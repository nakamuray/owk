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
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

tests = $(testGroupGenerator)


case_sum = do
    ret <- testOwkStringMap script_sum $ map (Number . fromInteger) [1..10]
    ret @?= [Number 55]
  where
    script_sum = [s|
        sum = ref 0
        end = {
          put $ sum ()
        }
        {
          sum := sum () + _
        }
        |]

case_sum_fold = do
    ret <- testOwkStringMap script_sum "0" $ map (Number . fromInteger) [1..10]
    ret @?= [Number 55]
  where
    script_sum = [s|
        fold (acc -> _ -> acc + _) 0
        |]

case_tail = do
    ret <- testOwkStringMap script_tail $ map (Number . fromInteger) [1..10]
    ret @?= map (Number . fromInteger) [2..10]
  where
    script_tail = [s|
        # read and ignore first object
        get ()
        main = put
        |]

case_curry = do
    ret <- flip testOwkString [] $ [s|
        f = x -> y -> put $ x * y
        f 2 3
    |]
    ret @?= [Number 6]

case_readme_string = do
    ret <- flip testOwkString [] [s|
        s = "hello, owk"
        put "\u3042"
    |]
    ret @?= [String "あ"]

case_readme_number = do
    ret <- flip testOwkString [] [s|
        i = 2
        j = 10.1
        put (i * j)
    |]
    ret @?= [Number 20.2]

case_readme_bool = do
    ret <- flip testOwkString [] [s|
        t = true
        f = false
        put(t, f)
    |]
    ret @?= [Tuple [Bool True, Bool False]]

case_readme_dict = do
    ret <- flip testOwkString [] [s|
        d = { key : "value", key2 : 100 }
        put d.key # => value
        put (d ["key"]) # => value
        d2 = d { key2 : 200, key3 : "spam" }
        d3 = d2 ["key4", "egg"]
        put(d2.key2, d3.key4)
    |]
    ret @?= [String "value", String "value", Tuple [Number 200, String "egg"]]

case_readme_list = do
    ret <- flip testOwkString [] [s|
        L = [1, 2, 3, "4"]
        put (L[0]) # => 1
        L2 = L[1, 2]
        put L2 # => [2, 3]
    |]
    ret @?= [Number 1, List (V.fromList [Number 2, Number 3])]

case_readme_tuple = do
    ret <- flip testOwkString [] [s|
        t = (1, 2)
        put (t, undef)
    |]
    ret @?= [Tuple [Tuple [Number 1, Number 2], Undef]]

case_readme_function = do
    ret <- flip testOwkString [] [s|
        f = { put "hi" }
        f () # => hi

        f2 = _ -> put "hi"
        f2 () # => hi

        f3 = name -> { put("hi,", name) }
        f3 "nakamuray" # => hi, nakamuray

        f4 = (x, y) -> { x * y }
        put (f4(2, 3))

        f5 = x -> y -> { x * y }
        put (f5 2 3)

        f6 = i -> { i * 2 }
        put (f6 10) # => 20

        f7 = { _ * 2 }
        put (f7 10) # => 20

        f8 = 0 -> { "zero" } | n -> { n }
        put (f8 0) # => zero
        put (f8 100) # => 100

        f9 = n (n > 5) -> "greater than five" | n -> "less than equal five"
        put (f9 5) # => less than equal five
        put (f9 6) # => greater than five
    |]
    ret @?= [ String "hi"
            , String "hi"
            , Tuple [String "hi,", String "nakamuray"]
            , Number 6
            , Number 6
            , Number 20
            , Number 20
            , String "zero"
            , Number 100
            , String "less than equal five"
            , String "greater than five"
            ]

case_readme_ref = do
    ret <- flip testOwkString [] [s|
        r = ref 0
        put (r ()) # => 0
        r := 1
        put (r ()) # => 1
    |]
    ret @?= [Number 0, Number 1]

case_readme_pattern = do
    ret <- flip testOwkString [] [s|
        (a, b) = (1, 2)
        [c, [d, e]] = [3, [4, 5]]
        f = (("6", 7) = ("6", 7))
        { key1 : g, key2 : h } = { key1 : 8, key2 : 9 }
        put (a, b, c, d, e, f, g, h)

        ((i, j) -> { put (i, j) }) (10, 11)

        { key1 : k } = { key1 : 12, key3 : 13 }

        l@{ key : m } = { key : "value", key2 : "value!" }
        put l # => { key : "value", key2 : "value!" }
        put m # => value

        n = (0 = 1)

        put (k, n)

        func = 0 -> { 0 } | 1 -> { 1 }
        put (func 1) # => 1
        put (func 2) # =>
    |]
    ret @?= [ Tuple [Number 1, Number 2, Number 3, Number 4, Number 5, Tuple [String "6", Number 7], Number 8, Number 9]
            , Tuple [Number 10, Number 11]
            , Dict $ H.fromList [("key", String "value"), ("key2", String "value!")]
            , String "value"
            , Tuple [Number 12, Undef]
            , Number 1
            , Undef
            ]

case_readme_operator_app = do
    ret <- flip testOwkString [] [s|
        put $ 1 + 1 # => 2
        put (1 + 1) # => 2
    |]
    ret @?= [Number 2, Number 2]

case_readme_operator_if = do
    ret <- flip testOwkString [] [s|
        true ? { put "hi" } # => hi
        false ? { put "hi?" }
    |]
    ret @?= [String "hi"]

case_readme_operator_variable = do
    ret <- flip testOwkString [] [s|
        put $ `+` 1 2  # => 3

        `+:` = x -> y -> put (x, "plus", y)
        1 +: 2  # => 1 plus 2
    |]
    ret @?= [Number 3, Tuple [Number 1, String "plus", Number 2]]


testOwkString :: T.Text -> [Object] -> IO [Object]
testOwkString script inputs = CL.sourceList inputs $= owkString script $$ CL.consume

testOwkStringMap :: T.Text -> [Object] -> IO [Object]
testOwkStringMap script inputs = CL.sourceList inputs $= owkStringMap script $$ CL.consume
