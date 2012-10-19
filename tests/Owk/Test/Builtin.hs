{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Owk.Test.Builtin where

import Test.Framework.TH
import Test.HUnit

import Data.Text ()
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.Vector as V

import Owk.Builtin
import Owk.Type
import Owk.Test.Util

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
