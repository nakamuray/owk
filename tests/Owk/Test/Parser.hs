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

case_unit_0 = parseOwk "<test>" "()" @?= Right (Program [Tuple []])

case_string_0 = parseOwk "<test>" "\"hello world\"" @?= Right (Program [String "hello world"])
case_string_1 = parseOwk "<test>" "\"\\u3042\"" @?= Right (Program [String "あ"])

case_number_0 = parseOwk "<test>" "100" @?= Right (Program [Number (I 100)])
case_number_1 = parseOwk "<test>" "100.0" @?= Right (Program [Number (D 100.0)])
case_number_2 = parseOwk "<test>" "-100" @?= Right (Program [FuncCall (Variable "__neg__") (Number (I 100))])

case_list_0 = parseOwk "<test>" "[]" @?= Right (Program [List []])
case_list_1 = parseOwk "<test>" "[1]" @?= Right (Program [List [Number (I 1)]])
case_list_2 = parseOwk "<test>" "[1, 2]" @?= Right (Program [List [Number (I 1), Number (I 2)]])

case_dict_0 = parseOwk "<test>" "{}" @?= Right (Program [Dict []])
case_dict_1 = parseOwk "<test>" "{key => \"value\"}" @?= Right (Program [Dict [("key", String "value")]])
case_dict_2 = parseOwk "<test>" "{key => \"value\", otherkey => \"other value\"}" @?= Right (Program [Dict [("key", String "value"), ("otherkey", String "other value")]])

case_function_0 = parseOwk "<test>" "{ 1 }" @?= Right (Program [Function [(PVariable "_", Nothing, [Number (I 1)])]])
case_function_1 = parseOwk "<test>" "{ 1; 2 }" @?= Right (Program [Function [(PVariable "_", Nothing, [Number (I 1), Number (I 2)])]])
case_function_2 = parseOwk "<test>" "(x, y) -> { 1 }" @?= Right (Program [Function [(PTuple [PVariable "x", PVariable "y"], Nothing, [Number (I 1)])]])
case_function_3 = parseOwk "<test>" "0 -> { \"zero\" } | 1 -> { \"one\" } | n -> { n }"
                      @?= Right (Program [Function [(PNumber (I 0), Nothing, [String "zero"]), (PNumber (I 1), Nothing, [String "one"]), (PVariable "n", Nothing, [Variable "n"])]])
case_function_4 = parseOwk "<test>" "f x y" @?= Right (Program [FuncCall (FuncCall (Variable "f") (Variable "x")) (Variable "y")])
case_function_5 = parseOwk "<test>" "_ -> 1" @?= Right (Program [Function [(PVariable "_", Nothing, [Number (I 1)])]])
case_function_6 = parseOwk "<test>" "x -> print x" @?= Right (Program [Function [(PVariable "x", Nothing, [FuncCall (Variable "print") (Variable "x")])]])
case_function_7 = parseOwk "<test>" "x -> y -> 1" @?= Right (Program [Function [(PVariable "x", Nothing, [Function [(PVariable "y", Nothing, [Number (I 1)])]])]])

case_function_guard_0 = parseOwk "<test>" "x (x > 10) -> x" @?= Right (Program [Function [(PVariable "x", Just (FuncCall (FuncCall (Variable ">") (Variable "x")) (Number (I 10))), [Variable "x"])]])

case_variable_0 = parseOwk "<test>" "x" @?= Right (Program [Variable "x"])

case_define_0 = parseOwk "<test>" "x = 42" @?= Right (Program [Define (PVariable "x") (Number (I 42))])
case_define_1 = parseOwk "<test>" "x = { 42 }" @?= Right (Program [Define (PVariable "x") (Function [(PVariable "_", Nothing, [(Number (I 42))])])])
case_define_2 = parseOwk "<test>" "x = $ -> { 42 }" @?= Right (Program [Define (PVariable "x") (Function [(PVariable "$", Nothing, [(Number (I 42))])])])
case_define_3 = parseOwk "<test>" "[x, y] = [1, 2]" @?= Right (Program [Define (PList [PVariable "x", PVariable "y"]) (List [Number (I 1), Number (I 2)])])
case_define_4 = parseOwk "<test>" "[1, y] = [1, 2]" @?= Right (Program [Define (PList [PNumber (I 1), PVariable "y"]) (List [Number (I 1), Number (I 2)])])
case_define_5 = parseOwk "<test>" "{ user => { name => n } } = x" @?= Right (Program [Define (PDict [("user", PDict [("name", PVariable "n")])]) (Variable "x")])
case_define_6 = parseOwk "<test>" "0 = 0" @?= Right (Program [Define (PNumber (I 0)) (Number (I 0))])

case_operator_0 = parseOwk "<test>" "1 + 2" @?= Right (Program [FuncCall (FuncCall (Variable "+") (Number (I 1))) (Number (I 2))])
case_operator_1 = parseOwk "<test>" "`+` 1 2" @?= Right (Program [FuncCall (FuncCall (Variable "+") (Number (I 1))) (Number (I 2))])
case_opratorr_2 = parseOwk "<test>" "`+:` = undef" @?= Right (Program [Define (PVariable "+:") (Variable "undef")])
case_opratorr_3 = parseOwk "<test>" "1 +: 2 *: 3" @?= Right (Program [FuncCall (FuncCall (Variable "+:") (Number (I 1))) (FuncCall (FuncCall (Variable "*:") (Number (I 2))) (Number (I 3)))])
case_opratorr_4 = parseOwk "<test>" "1 *: 2 +: 3" @?= Right (Program [FuncCall (FuncCall (Variable "+:") (FuncCall (FuncCall (Variable "*:") (Number (I 1))) (Number (I 2)))) (Number (I 3))])

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
case_newline_14 = parseOwk "<test>" "f x\ny" @?= Right (Program [FuncCall (Variable "f") (Variable "x"), Variable "y"])
case_newline_15 = parseOwk "<test>" "1 + \n2\n3" @?= Right (Program [FuncCall (FuncCall (Variable "+") (Number (I 1))) (Number (I 2)), Number (I 3)])
case_newline_16 = parseOwk "<test>" "0 -> 0\n| 1 -> 1\n2 -> 2" @?= Right (Program [Function [(PNumber (I 0), Nothing, [Number (I 0)]), (PNumber (I 1), Nothing, [Number (I 1)])], Function [(PNumber (I 2), Nothing, [Number (I 2)])]])
case_newline_17 = parseOwk "<test>" "0 -> 0|\n 1 -> 1\n2 -> 2" @?= Right (Program [Function [(PNumber (I 0), Nothing, [Number (I 0)]), (PNumber (I 1), Nothing, [Number (I 1)])], Function [(PNumber (I 2), Nothing, [Number (I 2)])]])
