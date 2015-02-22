{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.Owk
  ( owkString
  , owkFile
  , owk
  , owkEval
  ) where

import Data.Conduit

import Control.Applicative ((<$>))
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as TI

import qualified Owk.AST as AST
import Owk.Builtin (builtins)
import Owk.Interpreter
import Owk.Module.Time (time)
import Owk.Parser (parseOwk)
import Owk.Type
import qualified Owk.Namespace as Namespace


owkString :: Text -> Source IO Object -> Source IO Object
owkString script src = owk "<string>" script src

owkFile :: FilePath -> Source IO Object -> Source IO Object
owkFile fname src = do
    script <- liftIO $ TI.readFile fname
    owk fname script src

-- FIXME: make it Conduit, not require Source
owk :: String -> Text -> Source IO Object -> Source IO Object
owk fname script src =
    case parseOwk fname script of
        -- TODO: don't use error, use conduit's error system
        Left e     -> error e
        Right prog -> do
            n <- liftIO $ Namespace.fromList globalNamespace
            main <- liftIO $ runOwk'' fname prog n
            let src' = transPipe liftIO src
            o <- liftIO $ runOwk' (funcCall main (Stream src')) n
            case o of
                Stream s -> transPipe (\o' -> runOwk' o' n) s
                _        -> yield o

owkEval :: String -> Text -> Source IO Object -> Source IO Object
owkEval fname script _ =  -- ignore src
    case parseOwk fname script of
        -- TODO: don't use error, use conduit's error system
        Left e     -> error e
        Right prog -> do
            n <- liftIO $ Namespace.fromList globalNamespace
            o <- liftIO $ runOwk'' fname prog n
            case o of
                Stream s -> transPipe (\o' -> runOwk' o' n) s
                _        -> yield o

-- run program and return last expression as a main function
runOwk'' :: String -> AST.Program -> Namespace.Namespace -> IO Object
runOwk'' fname prog n = flip runOwk' n $ do
    g <- Namespace.extractGlobal <$> askScope
    s <- liftIO $ Namespace.create g
    main <- localScope s $ do
        Namespace.define "__file__" $ String (T.pack fname)
        interpret prog
    return main

-- TODO: load haskell modules at runtime
globalNamespace :: [(Text, Object)]
globalNamespace = builtins ++ [("time", time)]
