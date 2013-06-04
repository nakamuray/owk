{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Owk.Test.Builtin where

import Test.Framework.TH
import Test.HUnit

import Data.Text ()
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Owk.Builtin
import Owk.Builtin.Expand
import Owk.Type
import Owk.Test.Util
import qualified Owk.Namespace as Namespace

tests = $(testGroupGenerator)


case_and_1 = __and__ (Bool True) (Bool True) @=? Bool True
case_and_2 = __and__ (Bool False) (Bool True) @=? Bool False
case_and_3 = __and__ (Bool True) (Bool False) @=? Bool False
case_and_4 = __and__ (Bool False) (Bool False) @=? Bool False
case_and_5 = __and__ (Bool True) Undef @=? Undef
case_and_6 = __and__ (Bool True) (String "") @=? String ""
case_and_7 = __and__ (String "") (Bool True) @=? Bool True

case_or_1 = __or__ (Bool True) (Bool True) @=? Bool True
case_or_2 = __or__ (Bool False) (Bool True) @=? Bool True
case_or_3 = __or__ (Bool True) (Bool False) @=? Bool True
case_or_4 = __or__ (Bool False) (Bool False) @=? Bool False
case_or_5 = __or__ (Bool True) Undef @=? Bool True
case_or_6 = __or__ (Bool True) (String "") @=? Bool True
case_or_7 = __or__ (String "") (Bool True) @=? String ""

case_add_1 = testOwk_ $ do
    ret <- __add__ (String "hello") (String " world")
    liftIO $ ret @?= (String "hello world")
case_add_2 = testOwk_ $ do
    ret <- __add__ (Number (I 10)) (Number (I 20))
    liftIO $ ret @?= (Number (I 30))
case_add_3 = testOwk_ $ do
    ret <- __add__ (List $ V.fromList $ map (Number . I) [1, 2, 3]) (List $ V.fromList $ map (Number . I) [4, 5, 6])
    liftIO $ ret @?= (List $ V.fromList $ map (Number . I) [1, 2, 3, 4, 5, 6])

case_expand_1 = testOwk_ $ do
    Namespace.define "x" (String "hello")
    Namespace.define "y" (String "world")
    ret <- expand (String "#{x} #{y}. \\#{x}")
    liftIO $ ret @?= (String "hello world. #{x}")

case_expand_2 = testOwk_ $ do
    Namespace.define "o" (Dict $ H.fromList [("x", String "world")])
    ret <- expand (String "hello #{o.x}")
    liftIO $ ret @?= (String "hello world")

case_expand_3 = testOwk_ $ do
    Namespace.define "x" (Number (I 2))
    Namespace.define "y" (Number (I 3))
    ret <- expand (String "hello #{x * y}")
    liftIO $ ret @?= (String "hello 6")


case_split_1 = split (String " ") (String "hello world") @=? List (V.fromList [String "hello", String "world"])
case_split_2 = split (String "!") (String "hello world") @=? List (V.fromList [String "hello world"])

case_length_1 = testOwk_ $ do
    ret <- length_ (String "hello")
    liftIO $ ret @=? Number (I 5)
case_length_2 = testOwk_ $ do
    ret <- length_ (List $ V.fromList [Undef, Undef, Undef])
    liftIO $ ret @=? Number (I 3)
case_length_3 = testOwk_ $ do
    ret <- length_ (Dict $ H.fromList [("key", String "value"), ("number", Number (I 5))])
    liftIO $ ret @=? Number (I 2)
