{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Owk.Test.Type where

import Test.Framework.TH
import Test.HUnit

import Data.Text ()
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Owk.Builtin
import Owk.Type

import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Vector as V

tests = $(testGroupGenerator)


case_bool_1 = bool (Bool True) @=? Bool True
case_bool_2 = bool (Bool False) @=? Bool False
case_bool_3 = bool (String "") @=? Bool True
case_bool_4 = bool (String "hi") @=? Bool True
case_bool_5 = bool (Number $ I 0) @=? Bool True
case_bool_6 = bool (Number $ I 1) @=? Bool True
case_bool_7 = bool (Dict H.empty) @=? Bool True
case_bool_8 = bool (List V.empty) @=? Bool True
case_bool_9 = bool Undef @=? Bool False

case_list_0 = let orig = List $ V.fromList [String "x", String "y", String "z"]
              in list orig @=? orig
case_list_1 = list Undef @=? List V.empty
case_list_2 =
  let List v = list (Dict $ H.fromList [("key1", String "x"), ("key2", String "y"), ("key3", String "z")])
  in L.sort (V.toList v) @=? [String "key1", String "key2", String "key3"]
case_list_3 = list (String "xyz") @=? List (V.fromList [String "x", String "y", String "z"])

_types = [Ref undefined, unit, String "", List V.empty, Dict H.empty, Number (I 0), Bool True, Undef]
case_eq_0 = assertBool "each type should no equal" $ and [x /= y | (x, y) <- zip _types $ tail _types]
case_ord_0 = assertBool "type ordering" $ and [x > y | (x, y) <- zip _types $ tail _types]
