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

case_empty_0 = parseOwk "<test>" "" @?= Right (Program [])
case_empty_1 = parseOwk "<test>" "\n\n" @?= Right (Program [])
case_empty_2 = parseOwk "<test>" "# comment only" @?= Right (Program [])

case_unit_0 = parseOwk "<test>" "()" @?= Right (Program [Undef])

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

case_define_0 = parseOwk "<test>" "x = 42" @?= Right (Program [Define (PVariable "x") (Number (I 42))])
case_define_1 = parseOwk "<test>" "x = { 42 }" @?= Right (Program [Define (PVariable "x") (Function [] [(Number (I 42))])])
case_define_2 = parseOwk "<test>" "x = $ -> { 42 }" @?= Right (Program [Define (PVariable "x") (Function ["$"] [(Number (I 42))])])
case_define_3 = parseOwk "<test>" "[x, y] = [1, 2]" @?= Right (Program [Define (PList [PVariable "x", PVariable "y"]) (List [Number (I 1), Number (I 2)])])
case_define_4 = parseOwk "<test>" "[1, y] = [1, 2]" @?= Right (Program [Define (PList [PNumber (I 1), PVariable "y"]) (List [Number (I 1), Number (I 2)])])
case_define_5 = parseOwk "<test>" "{ user => { name => n } } = x" @?= Right (Program [Define (PDict [("user", PDict [("name", PVariable "n")])]) (Variable "x")])

case_operator_0 = parseOwk "<test>" "1 + 2" @?= Right (Program [FuncCall (Variable "__add__") [Number (I 1), Number (I 2)]])

case_newline_0 = parseOwk "<test>" "1; 2; 3" @=? parseOwk "<test>" "1\n2\n3\n"
case_newline_1 = parseOwk "<test>" "1; 2; 3" @=? parseOwk "<test>" "1\n\n2\n\n3\n"
case_newline_2 = parseOwk "<test>" "1; 2 3" @=? parseOwk "<test>" "1\n2\\\n3\n"
case_newline_3 = parseOwk "<test>" "1; 2 3" @=? parseOwk "<test>" "1\n2 \\\n 3\n"
case_newline_4 = parseOwk "<test>" "1; 2; 3" @=? parseOwk "<test>" "1;\n2;\n3\n"

case_newline_5 = parseOwk "<test>" "{ 1; 2; 3 }" @=? parseOwk "<test>" "{ 1\n2\n3\n}"
case_newline_6 = parseOwk "<test>" "{ 1; 2; 3 }" @=? parseOwk "<test>" "{ 1\n\n2\n\n3\n}"
case_newline_7 = parseOwk "<test>" "{ 1; 2 3 }" @=? parseOwk "<test>" "{ 1\n2\\\n3\n}"
case_newline_8 = parseOwk "<test>" "{ 1; 2 3 }" @=? parseOwk "<test>" "{ 1\n2 \\\n 3\n}"
case_newline_9 = parseOwk "<test>" "{ 1; 2; 3 }" @=? parseOwk "<test>" "{ 1;\n2;\n3\n}"

case_newline_10 = parseOwk "<test>" "1;2;3" @=? parseOwk "<test>" " 1;\n  2;\n  3\n"
case_newline_11 = parseOwk "<test>" "\"x\";\"y\";\"z\"" @=? parseOwk "<test>" "\"x\"\n\"y\"\n\"z\"\n"

case_newline_12 = parseOwk "<test>" "[\n  1,\n  2 , \n  3\n]" @?= Right (Program [List [Number (I 1), Number (I 2), Number (I 3)]])
case_newline_13 = parseOwk "<test>" "{\n  k1 => 1,\n  k2 \n => \n 2 , \n  k3 => 3\n}" @?= Right (Program [Dict [("k1", Number (I 1)), ("k2", Number (I 2)), ("k3", Number (I 3))]])
