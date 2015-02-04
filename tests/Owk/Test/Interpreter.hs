{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Owk.Test.Interpreter where

import Test.Framework.TH
import Test.HUnit

import Data.Text ()
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Owk.AST as AST
import Owk.Interpreter
import Owk.Type as Type
import Owk.Test.Util

import qualified Owk.Namespace as Namespace

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
    x <- funcCall (Type.List $ V.fromList [Type.String "x", Type.String "y", Type.String "z"]) (Type.Number 0)
    liftIO $ x @?= Type.String "x"
    y <- funcCall (Type.List $ V.fromList [Type.String "x", Type.String "y", Type.String "z"]) (Type.Number 1)
    liftIO $ y @?= Type.String "y"
    u <- funcCall (Type.List $ V.fromList [Type.String "x", Type.String "y", Type.String "z"]) (Type.Number 10)
    liftIO $ u @?= Type.Undef

case_list_3 = testOwk_ $ do
    x <- funcCall (Type.List $ V.fromList [Type.String "x", Type.String "y", Type.String "z"]) (Type.List (V.fromList [Type.Number 0]))
    liftIO $ x @?= Type.String "x"

case_dict_0 = testOwk_ $ do
    d <- expr $ AST.Dict []
    liftIO $ d @?= Type.Dict H.empty

case_dict_1 = testOwk_ $ do
    let d1 = Type.Dict $ H.fromList [("key1", Type.String "value1"), ("key2", Type.String "value2")]
        d2 = Type.Dict $ H.fromList [("key2", Type.String "VALUE2"), ("key3", Type.String "VALUE3")]
    d <- funcCall d1 d2
    liftIO $ d @?= Type.Dict (H.fromList [("key2", Type.String "VALUE2"), ("key3", Type.String "VALUE3"), ("key1", Type.String "value1")])

case_dict_2 = testOwk_ $ do
    let d = Type.Dict $ H.fromList [("key1", Type.String "value1"), ("key2", Type.String "value2")]
    d' <- funcCall d $ Type.List $ V.fromList [Type.String "key3", Type.String "value3"]
    liftIO $ d' @?= Type.Dict (H.fromList [("key2", Type.String "value2"), ("key3", Type.String "value3"), ("key1", Type.String "value1")])

case_dict_3 = testOwk_ $ do
    let d = Type.Dict $ H.fromList [("key1", Type.String "value1"), ("key2", Type.String "value2")]
    v <- funcCall d $ Type.String "key1"
    liftIO $ v @?= Type.String "value1"

case_dict_4 = testOwk_ $ do
    let d = Type.Dict $ H.fromList [("key1", Type.String "value1"), ("key2", Type.String "value2")]
    v <- funcCall d $ Type.String "key3"
    liftIO $ v @?= Type.Undef

case_define_1 = testOwk_ $ do
    ret <- expr $ Define (PVariable "x" Nothing) (AST.String "hello")
    v <- Namespace.lookup "x"
    liftIO $ ret @?= Type.String "hello"
    liftIO $ v @?= Type.String "hello"

case_define_2 = testOwk_ $ do
    ret <- expr $ Define (PVariable "x" Nothing) (AST.String "hello")
    x <- Namespace.lookup "x"
    liftIO $ ret @?= Type.String "hello"
    liftIO $ x @?= Type.String "hello"

case_define_3 = testOwk_ $ do
    ret <- expr $ Define (PList [PVariable "x" Nothing, PVariable "y" Nothing]) (AST.List [AST.String "hello", AST.String "world"])
    x <- Namespace.lookup "x"
    y <- Namespace.lookup "y"
    liftIO $ ret @?= Type.List (V.fromList [Type.String "hello", Type.String "world"])
    liftIO $ x @?= Type.String "hello"
    liftIO $ y @?= Type.String "world"

case_define_4 = testOwk_ $ do
    ret <- expr $ Define (PList [PVariable "x" Nothing]) (AST.List [AST.String "hello", AST.String "world"])
    x <- Namespace.lookup "x"
    liftIO $ ret @?= Type.Undef
    liftIO $ x @?= Type.Undef

case_define_5 = testOwk_ $ do
    ret <- expr $ Define (PList [PVariable "x" Nothing, PString "world"]) (AST.List [AST.String "hello", AST.String "world"])
    x <- Namespace.lookup "x"
    liftIO $ ret @?= Type.List (V.fromList [Type.String "hello", Type.String "world"])
    liftIO $ x @?= Type.String "hello"

case_define_6 = testOwk_ $ do
    ret <- expr $ Define (PList [PVariable "x" Nothing, PString "world!"]) (AST.List [AST.String "hello", AST.String "world"])
    x <- Namespace.lookup "x"
    liftIO $ ret @?= Type.Undef
    liftIO $ x @?= Type.Undef

case_define_7 = testOwk_ $ do
    ret <- expr $ Define (PDict [("kx", PVariable "x" Nothing), ("ky", PVariable "y" Nothing)]) (AST.Dict [("kx", AST.String "hello"), ("ky", AST.String "world")])
    x <- Namespace.lookup "x"
    y <- Namespace.lookup "y"
    liftIO $ ret @?= Type.Dict (H.fromList [("kx", Type.String "hello"), ("ky", Type.String "world")])
    liftIO $ x @?= Type.String "hello"
    liftIO $ y @?= Type.String "world"

case_define_8 = testOwk_ $ do
    ret <- expr $ Define (PDict [("user", PDict [("name", PVariable "n" Nothing)])])
                         (AST.Dict [("user", AST.Dict [("name", AST.String "nakamuray"), ("value", AST.Number 100)]), ("text", AST.String "hello world")])
    n <- Namespace.lookup "n"
    liftIO $ ret @?= (Type.Dict $ H.fromList [("user", Type.Dict $ H.fromList [("name", Type.String "nakamuray"), ("value", Type.Number 100)]), ("text", Type.String "hello world")])
    liftIO $ n @?= Type.String "nakamuray"

case_define_9 = testOwk_ $ do
    ret1 <- expr $ Define (PVariable "x" (Just (PNumber 1))) (AST.Number 1)
    ret2 <- expr $ Define (PVariable "y" (Just (PNumber 1))) (AST.Number 2)
    x <- Namespace.lookup "x"
    y <- Namespace.lookup "y"
    liftIO $ ret1 @?= Type.Number 1
    liftIO $ ret2 @?= Type.Undef
    liftIO $ x @?= Type.Number 1
    liftIO $ y @?= Type.Undef
