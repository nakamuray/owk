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

case_curry = do
    ret <- flip testOwkString [] $ [s|
        f = x -> y -> print : x * y
        f 2 3
    |]
    ret @?= [[Number (I 6)]]

case_readme_string = do
    ret <- flip testOwkString [] [s|
        s = "hello, owk"
        print "\u3042"
    |]
    ret @?= [[String "あ"]]

case_readme_number = do
    ret <- flip testOwkString [] [s|
        i = 2
        j = 10.1
        print (i * j)
    |]
    ret @?= [[Number (D 20.2)]]

case_readme_bool = do
    ret <- flip testOwkString [] [s|
        t = true
        f = false
        print(t, f)
    |]
    ret @?= [[Bool True, Bool False]]

case_readme_dict = do
    ret <- flip testOwkString [] [s|
        d = { key => "value", key2 => 100 }
        print d.key # => value
        print (d ["key"]) # => value
        d2 = d { key2 => 200, key3 => "spam" }
        d3 = d2 ["key4", "egg"]
        print(d2.key2, d3.key4)
    |]
    ret @?= [[String "value"], [String "value"], [Number (I 200), String "egg"]]

case_readme_list = do
    ret <- flip testOwkString [] [s|
        L = [1, 2, 3, "4"]
        print (L[0]) # => 1
        L2 = L[1, 2]
        print L2 # => [2, 3]
    |]
    ret @?= [[Number (I 1)], [List (V.fromList [Number (I 2), Number (I 3)])]]

case_readme_tuple = do
    ret <- flip testOwkString [] [s|
        t = (1, 2)
        print (t, undef)
    |]
    ret @?= [[Tuple [Number (I 1), Number (I 2)], Undef]]

case_readme_function = do
    ret <- flip testOwkString [] [s|
        f = { print "hi" }
        f () # => hi

        f2 = name -> { print("hi,", name) }
        f2 "nakamuray" # => hi, nakamuray

        f3 = (x, y) -> { x * y }
        print (f3(2, 3))

        f4 = i -> { i * 2 }
        print (f4 10) # => 20

        f5 = { _ * 2 }
        print (f5 10) # => 20

        f6 = 0 -> { "zero" } | n -> { n }
        print (f6 0) # => zero
        print (f6 100) # => 100
    |]
    ret @?= [ [String "hi"]
            , [String "hi,", String "nakamuray"]
            , [Number (I 6)]
            , [Number (I 20)]
            , [Number (I 20)]
            , [String "zero"]
            , [Number (I 100)]
            ]

case_readme_ref = do
    ret <- flip testOwkString [] [s|
        r = ref 0
        print (r ()) # => 0
        r := 1
        print (r ()) # => 1
    |]
    ret @?= [[Number (I 0)], [Number (I 1)]]

case_readme_pattern = do
    ret <- flip testOwkString [] [s|
        (a, b) = (1, 2)
        [c, [d, e]] = [3, [4, 5]]
        f = (("6", 7) = ("6", 7))
        { key1 => g, key2 => h } = { key1 => 8, key2 => 9 }
        print (a, b, c, d, e, f, g, h)

        ((i, j) -> { print (i, j) }) (10, 11)

        { key1 => k } = { key1 => 12, key3 => 13 }

        l = (0 = 1)

        print (k, l)

        func = 0 -> { 0 } | 1 -> { 1 }
        print (func 1) # => 1
        print (func 2) # =>
    |]
    ret @?= [ [Number (I 1), Number (I 2), Number (I 3), Number (I 4), Number (I 5), Tuple [String "6", Number (I 7)], Number (I 8), Number (I 9)]
            , [Number (I 10), Number (I 11)]
            , [Number (I 12), Undef]
            , [Number (I 1)]
            , [Undef]
            ]

case_readme_operator_app = do
    ret <- flip testOwkString [] [s|
        print : 1 + 1 # => 2
        print (1 + 1) # => 2
    |]
    ret @?= [[Number (I 2)], [Number (I 2)]]

case_readme_operator_if = do
    ret <- flip testOwkString [] [s|
        true ? { print "hi" } # => hi
        false ? { print "hi?" }
    |]
    ret @?= [[String "hi"]]


testOwkString :: T.Text -> [Object] -> IO [[Object]]
testOwkString script inputs = CL.sourceList inputs $= owkString script $$ CL.consume
