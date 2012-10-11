{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Owk.Test.Parser where

import Test.Framework.TH
import Test.HUnit

import Data.Attoparsec.Number (Number(..))
import Data.Text ()
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Owk.AST
import Owk.Parser

tests = $(testGroupGenerator)


case_unit_0 = parseOwk "<test>" "()" @?= Right (Program [Unit])

case_string_0 = parseOwk "<test>" "\"hello world\"" @?= Right (Program [String "hello world"])
case_string_1 = parseOwk "<test>" "\"\\u3042\"" @?= Right (Program [String "あ"])

case_number_0 = parseOwk "<test>" "100" @?= Right (Program [Number (I 100)])
case_number_1 = parseOwk "<test>" "100.0" @?= Right (Program [Number (D 100.0)])
case_number_2 = parseOwk "<test>" "-100" @?= Right (Program [FuncCall (Variable "__neg__") [Number (I 100)]])

case_list_0 = parseOwk "<test>" "[]" @?= Right (Program [List []])
case_list_1 = parseOwk "<test>" "[1]" @?= Right (Program [List [Number (I 1)]])
case_list_2 = parseOwk "<test>" "[1, 2]" @?= Right (Program [List [Number (I 1), Number (I 2)]])

case_dict_0 = parseOwk "<test>" "{}" @?= Right (Program [Dict []])
case_dict_1 = parseOwk "<test>" "{key => \"value\"}" @?= Right (Program [Dict [("key", String "value")]])
case_dict_2 = parseOwk "<test>" "{key => \"value\", otherkey => \"other value\"}" @?= Right (Program [Dict [("key", String "value"), ("otherkey", String "other value")]])

case_function_0 = parseOwk "<test>" "{ 1 }" @?= Right (Program [Function [] [Number (I 1)]])
case_function_1 = parseOwk "<test>" "{ 1; 2 }" @?= Right (Program [Function [] [Number (I 1), Number (I 2)]])
case_function_2 = parseOwk "<test>" "x, y -> { 1 }" @?= Right (Program [Function ["x", "y"] [Number (I 1)]])

case_variable_0 = parseOwk "<test>" "x" @?= Right (Program [Variable "x"])

case_define_0 = parseOwk "<test>" "x = 42" @?= Right (Program [Define "x" (Number (I 42))])

case_operator_0 = parseOwk "<test>" "1 + 2" @?= Right (Program [FuncCall (Variable "__add__") [Number (I 1), Number (I 2)]])
