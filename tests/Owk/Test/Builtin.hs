{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Owk.Test.Builtin where

import Test.Framework.TH
import Test.HUnit

import Data.Text ()
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Owk.Builtin
import Owk.Type

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
