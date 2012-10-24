{-# LANGUAGE OverloadedStrings #-}
module Owk.Module (import_) where

import Control.Monad.Reader (asks, local)

import qualified Data.Text as T

import Owk.Interpreter
import Owk.Type
import Owk.Parser (parseOwkFile)
import qualified Owk.Namespace as Namespace

import_ :: FilePath -> Owk Object
import_ fpath = do
    g <- asks Namespace.extractGlobal
    s <- liftIO $ Namespace.create g
    ret <- liftIO $ parseOwkFile fpath
    case ret of
        Left e  -> exception $ String $ T.pack e
        Right prog -> do
            local (const s) $ interpret_ prog
            h <- liftIO $ Namespace.toHash (Namespace.currentNamepace s)
            return $ Dict h

