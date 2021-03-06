{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Owk.Test.Parser where

import Test.Framework.TH
import Test.HUnit

import Control.Applicative ((<$>))
import Data.Text ()
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Text.Trifecta.Delta (Delta(Columns))

import Owk.AST
import Owk.Parser

tests = $(testGroupGenerator)

-- TODO: write tests to check FuncCall's Location

case_empty_0 = parseOwk "<test>" "" @?= Right (Program [])
case_empty_1 = parseOwk "<test>" "\n\n" @?= Right (Program [])
case_empty_2 = parseOwk "<test>" "# comment only" @?= Right (Program [])

case_unit_0 = parseOwk "<test>" "()" @?= Right (Program [Tuple []])

case_string_0 = parseOwk "<test>" "\"hello world\"" @?= Right (Program [String "hello world"])
case_string_1 = parseOwk "<test>" "\"\\u3042\"" @?= Right (Program [String "あ"])

case_number_0 = parseOwk "<test>" "100" @?= Right (Program [Number 100])
case_number_1 = parseOwk "<test>" "100.0" @?= Right (Program [Number 100.0])
case_number_2 = parseOwk' "<test>" "-100" @?= Right (Program [FuncCall _loc (Variable "__neg__") (Number 100)])

case_list_0 = parseOwk "<test>" "[]" @?= Right (Program [List []])
case_list_1 = parseOwk "<test>" "[1]" @?= Right (Program [List [Number 1]])
case_list_2 = parseOwk "<test>" "[1, 2]" @?= Right (Program [List [Number 1, Number 2]])

case_dict_0 = parseOwk "<test>" "{}" @?= Right (Program [Dict []])
case_dict_1 = parseOwk "<test>" "{key : \"value\"}" @?= Right (Program [Dict [("key", String "value")]])
case_dict_2 = parseOwk "<test>" "{key : \"value\", otherkey : \"other value\"}" @?= Right (Program [Dict [("key", String "value"), ("otherkey", String "other value")]])

case_function_0 = parseOwk "<test>" "{ 1 }" @?= Right (Program [Function [(PVariable "_" Nothing, Nothing, [Number 1])]])
case_function_1 = parseOwk "<test>" "{ 1; 2 }" @?= Right (Program [Function [(PVariable "_" Nothing, Nothing, [Number 1, Number 2])]])
case_function_2 = parseOwk "<test>" "(x, y) -> { 1 }" @?= Right (Program [Function [(PTuple [PVariable "x" Nothing, PVariable "y" Nothing], Nothing, [Number 1])]])
case_function_3 = parseOwk "<test>" "0 -> { \"zero\" } | 1 -> { \"one\" } | n -> { n }"
                      @?= Right (Program [Function [(PNumber 0, Nothing, [String "zero"]), (PNumber 1, Nothing, [String "one"]), (PVariable "n" Nothing, Nothing, [Variable "n"])]])
case_function_4 = parseOwk' "<test>" "f x y" @?= Right (Program [FuncCall _loc (FuncCall _loc (Variable "f") (Variable "x")) (Variable "y")])
case_function_5 = parseOwk "<test>" "_ -> 1" @?= Right (Program [Function [(PVariable "_" Nothing, Nothing, [Number 1])]])
case_function_6 = parseOwk' "<test>" "x -> print x" @?= Right (Program [Function [(PVariable "x" Nothing, Nothing, [FuncCall _loc (Variable "print") (Variable "x")])]])
case_function_7 = parseOwk "<test>" "x -> y -> 1" @?= Right (Program [Function [(PVariable "x" Nothing, Nothing, [Function [(PVariable "y" Nothing, Nothing, [Number 1])]])]])

case_function_guard_0 = parseOwk' "<test>" "x (x > 10) -> x" @?= Right (Program [Function [(PVariable "x" Nothing, Just (FuncCall _loc (FuncCall _loc (Variable ">") (Variable "x")) (Number 10)), [Variable "x"])]])

case_variable_0 = parseOwk "<test>" "x" @?= Right (Program [Variable "x"])

case_define_0 = parseOwk "<test>" "x = 42" @?= Right (Program [Define (PVariable "x" Nothing) (Number 42)])
case_define_1 = parseOwk "<test>" "x = { 42 }" @?= Right (Program [Define (PVariable "x" Nothing) (Function [(PVariable "_" Nothing, Nothing, [(Number 42)])])])
case_define_2 = parseOwk "<test>" "x = _ -> { 42 }" @?= Right (Program [Define (PVariable "x" Nothing) (Function [(PVariable "_" Nothing, Nothing, [(Number 42)])])])
case_define_3 = parseOwk "<test>" "[x, y] = [1, 2]" @?= Right (Program [Define (PList [PVariable "x" Nothing, PVariable "y" Nothing]) (List [Number 1, Number 2])])
case_define_4 = parseOwk "<test>" "[1, y] = [1, 2]" @?= Right (Program [Define (PList [PNumber 1, PVariable "y" Nothing]) (List [Number 1, Number 2])])
case_define_5 = parseOwk "<test>" "{ user : { name : n } } = x" @?= Right (Program [Define (PDict [("user", PDict [("name", PVariable "n" Nothing)])]) (Variable "x")])
case_define_6 = parseOwk "<test>" "0 = 0" @?= Right (Program [Define (PNumber 0) (Number 0)])
case_define_7 = parseOwk "<test>" "x@[1, y@2, {k:z@3}] = a" @?= Right (Program [Define (PVariable "x" (Just (PList [PNumber 1, PVariable "y" (Just (PNumber 2)), PDict [("k", PVariable "z" (Just (PNumber 3)))]]))) (Variable "a")])

case_operator_0 = parseOwk' "<test>" "1 + 2" @?= Right (Program [FuncCall _loc (FuncCall _loc (Variable "+") (Number 1)) (Number 2)])
case_operator_1 = parseOwk' "<test>" "`+` 1 2" @?= Right (Program [FuncCall _loc (FuncCall _loc (Variable "+") (Number 1)) (Number 2)])
case_opratorr_2 = parseOwk "<test>" "`+:` = undef" @?= Right (Program [Define (PVariable "+:" Nothing) (Variable "undef")])
case_opratorr_3 = parseOwk' "<test>" "1 +: 2 *: 3" @?= Right (Program [FuncCall _loc (FuncCall _loc (Variable "+:") (Number 1)) (FuncCall _loc (FuncCall _loc (Variable "*:") (Number 2)) (Number 3))])
case_opratorr_4 = parseOwk' "<test>" "1 *: 2 +: 3" @?= Right (Program [FuncCall _loc (FuncCall _loc (Variable "+:") (FuncCall _loc (FuncCall _loc (Variable "*:") (Number 1)) (Number 2))) (Number 3)])

case_newline_0 = parseOwk "<test>" "1; 2; 3" @=? parseOwk "<test>" "1\n2\n3\n"
case_newline_1 = parseOwk "<test>" "1; 2; 3" @=? parseOwk "<test>" "1\n\n2\n\n3\n"
case_newline_2 = parseOwk' "<test>" "1; 2 3" @=? parseOwk' "<test>" "1\n2\\\n3\n"
case_newline_3 = parseOwk' "<test>" "1; 2 3" @=? parseOwk' "<test>" "1\n2 \\\n 3\n"
case_newline_4 = parseOwk "<test>" "1; 2; 3" @=? parseOwk "<test>" "1;\n2;\n3\n"

case_newline_5 = parseOwk "<test>" "{ 1; 2; 3 }" @=? parseOwk "<test>" "{ 1\n2\n3\n}"
case_newline_6 = parseOwk "<test>" "{ 1; 2; 3 }" @=? parseOwk "<test>" "{ 1\n\n2\n\n3\n}"
case_newline_7 = parseOwk' "<test>" "{ 1; 2 3 }" @=? parseOwk' "<test>" "{ 1\n2\\\n3\n}"
case_newline_8 = parseOwk' "<test>" "{ 1; 2 3 }" @=? parseOwk' "<test>" "{ 1\n2 \\\n 3\n}"
case_newline_9 = parseOwk "<test>" "{ 1; 2; 3 }" @=? parseOwk "<test>" "{ 1;\n2;\n3\n}"

case_newline_10 = parseOwk "<test>" "1;2;3" @=? parseOwk "<test>" " 1;\n  2;\n  3\n"
case_newline_11 = parseOwk "<test>" "\"x\";\"y\";\"z\"" @=? parseOwk "<test>" "\"x\"\n\"y\"\n\"z\"\n"

case_newline_12 = parseOwk "<test>" "[\n  1,\n  2 , \n  3\n]" @?= Right (Program [List [Number 1, Number 2, Number 3]])
case_newline_13 = parseOwk "<test>" "{\n  k1 : 1,\n  k2 \n : \n 2 , \n  k3 : 3\n}" @?= Right (Program [Dict [("k1", Number 1), ("k2", Number 2), ("k3", Number 3)]])
case_newline_14 = parseOwk' "<test>" "f x\ny" @?= Right (Program [FuncCall _loc (Variable "f") (Variable "x"), Variable "y"])
case_newline_15 = parseOwk' "<test>" "1 + \n2\n3" @?= Right (Program [FuncCall _loc (FuncCall _loc (Variable "+") (Number 1)) (Number 2), Number 3])
case_newline_16 = parseOwk "<test>" "0 -> 0\n| 1 -> 1\n2 -> 2" @?= Right (Program [Function [(PNumber 0, Nothing, [Number 0]), (PNumber 1, Nothing, [Number 1])], Function [(PNumber 2, Nothing, [Number 2])]])
case_newline_17 = parseOwk "<test>" "0 -> 0|\n 1 -> 1\n2 -> 2" @?= Right (Program [Function [(PNumber 0, Nothing, [Number 0]), (PNumber 1, Nothing, [Number 1])], Function [(PNumber 2, Nothing, [Number 2])]])


-- XXX: dummy location
_loc = (Columns 0 0, "")

stripLocation (Right (Program es)) = Right $ Program $ map stripLocation' es
stripLocation e = e

stripLocation' (Function pgbs) = Function $ map (\(p, mg, es) -> (p, stripLocation' <$> mg, map stripLocation' es)) pgbs
stripLocation' (Define p e) = Define p $ stripLocation' e
stripLocation' (FuncCall l e1 e2) = FuncCall _loc (stripLocation' e1) (stripLocation' e2)
stripLocation' (Tuple es) = Tuple $ map stripLocation' es
stripLocation' (List es) = List $ map stripLocation' es
stripLocation' (Dict kvs) = Dict $ map (\(k, v) -> (k, stripLocation' v)) kvs
stripLocation' e = e

parseOwk' sn = stripLocation . parseOwk sn
