{-# LANGUAGE OverloadedStrings #-}
module Owk.Module
    ( import_
    , importProgram
    ) where

import Control.Applicative ((<$>))

import qualified Data.Text as T

import Owk.Interpreter
import Owk.Type
import Owk.AST (Program)
import Owk.Parser (parseOwkFile)
import qualified Owk.Namespace as Namespace

import_ :: FilePath -> Owk Object
import_ fpath = do
    ret <- liftIO $ parseOwkFile fpath
    case ret of
        Left e  -> exception e
        Right prog -> importProgram fpath prog

importProgram :: FilePath -> Program -> Owk Object
importProgram fpath prog = do
    g <- Namespace.extractGlobal <$> askScope
    s <- liftIO $ Namespace.create g
    localScope s $ do
        Namespace.define "__file__" $ String (T.pack fpath)
        interpret_ prog
    h <- liftIO $ Namespace.toHash (Namespace.currentNamepace s)
    return $ Dict h
