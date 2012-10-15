{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Owk.Test.Interpreter where

import Test.Framework.TH
import Test.HUnit

import Data.Conduit

import Data.Text ()
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Owk.AST as AST
import Owk.Interpreter
import Owk.Type as Type

import qualified Owk.Namespace as Namespace

import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

tests = $(testGroupGenerator)


case_string_0 = testOwk_ $ do
    s <- expr $ AST.String ""
    liftIO $ s @?= Type.String ""

case_list_0 = testOwk_ $ do
    l <- expr $ AST.List []
    liftIO $ l @?= Type.List V.empty

case_list_1 = testOwk_ $ do
    l <- expr $ AST.List [AST.String ""]
    liftIO $ l @?= Type.List (V.fromList [Type.String ""])

case_list_2 = testOwk_ $ do
    x <- funcCall (Type.List $ V.fromList [Type.String "x", Type.String "y", Type.String "z"]) [Type.Number $ I 0]
    liftIO $ x @?= Type.String "x"
    y <- funcCall (Type.List $ V.fromList [Type.String "x", Type.String "y", Type.String "z"]) [Type.Number $ I 1]
    liftIO $ y @?= Type.String "y"
    u <- funcCall (Type.List $ V.fromList [Type.String "x", Type.String "y", Type.String "z"]) [Type.Number $ I 10]
    liftIO $ u @?= Type.Undef

case_list_3 = testOwk_ $ do
    x <- funcCall (Type.List $ V.fromList [Type.String "x", Type.String "y", Type.String "z"]) [Type.List (V.fromList [Type.Number $ I 0])]
    liftIO $ x @?= Type.String "x"

case_dict_0 = testOwk_ $ do
    d <- expr $ AST.Dict []
    liftIO $ d @?= Type.Dict H.empty

case_dict_1 = testOwk_ $ do
    let d1 = Type.Dict $ H.fromList [("key1", Type.String "value1"), ("key2", Type.String "value2")]
        d2 = Type.Dict $ H.fromList [("key2", Type.String "VALUE2"), ("key3", Type.String "VALUE3")]
    d <- funcCall d1 [d2]
    liftIO $ d @?= Type.Dict (H.fromList [("key2", Type.String "VALUE2"), ("key3", Type.String "VALUE3"), ("key1", Type.String "value1")])

case_dict_2 = testOwk_ $ do
    let d = Type.Dict $ H.fromList [("key1", Type.String "value1"), ("key2", Type.String "value2")]
    d' <- funcCall d [Type.List $ V.fromList [Type.String "key3", Type.String "value3"]]
    liftIO $ d' @?= Type.Dict (H.fromList [("key2", Type.String "value2"), ("key3", Type.String "value3"), ("key1", Type.String "value1")])


testOwk owk = do
    n <- Namespace.fromList []
    CL.sourceNull $= runOwk owk n $$ CL.consume

testOwk_ owk = testOwk owk >> return ()
