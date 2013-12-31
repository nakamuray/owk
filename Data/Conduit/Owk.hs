{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.Owk
  ( owkString
  , owkStringMap
  , owkFile
  , owkFileMap
  , owk
  , owkMap
  ) where

import Data.Conduit

import Control.Monad.Error (catchError)
import Control.Monad.Reader (asks, local)
import Data.Text (Text)
import System.Exit (ExitCode(..), exitSuccess, exitWith)

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.IO as TI

import qualified Owk.AST as AST
import Owk.Builtin (builtins)
import Owk.Interpreter
import Owk.Module
import Owk.Module.Time (time)
import Owk.Parser (parseOwk)
import Owk.Type
import qualified Owk.Namespace as Namespace


owkString :: Text -> Conduit Object IO Object
owkString script = owk "<string>" script

owkStringMap :: Text -> Conduit Object IO Object
owkStringMap script = owkMap "<string>" script

owkFile :: FilePath -> Conduit Object IO Object
owkFile fname = do
    script <- liftIO $ TI.readFile fname
    owk fname script

owkFileMap :: FilePath -> Conduit Object IO Object
owkFileMap fname = do
    script <- liftIO $ TI.readFile fname
    owkMap fname script

owk :: String -> Text -> Conduit Object IO Object
owk fname script =
    case parseOwk fname script of
        -- TODO: don't use error, use conduit's error system
        Left e     -> error e
        Right prog -> do
            n <- liftIO $ Namespace.fromList globalNamespace
            runOwk (importProgram fname prog `catchError` catchExit) n

owkMap :: String -> Text -> Conduit Object IO Object
owkMap fname script =
    case parseOwk fname script of
        -- TODO: don't use error, use conduit's error system
        Left e     -> error e
        Right prog -> do
            n <- liftIO $ Namespace.fromList globalNamespace
            -- run script and get last expression as a main function
            (main, s) <- flip runOwk' n $ do
                g <- asks Namespace.extractGlobal
                s <- liftIO $ Namespace.create g
                main <- local (const s) $ do
                    Namespace.define "__file__" $ String (T.pack fname)
                    interpret prog `catchError` catchExit
                return (main, s)
            -- and then, run `main`
            awaitForever $ \obj -> do
                runOwk (funcCall main obj `catchError` ignoreNext `catchError` catchExit >> return ()) n
                return ()
            h <- liftIO $ Namespace.toHash (Namespace.currentNamepace s)
            case H.lookup "end" h of
                Just end -> do
                    runOwk (funcCall end unit `catchError` ignoreNext `catchError` catchExit >> return ()) n
                    return ()
                Nothing  -> return ()
  where
    ignoreNext Next = return Undef
    ignoreNext e    = throwError e

-- TODO: don't exit inside conduit
catchExit (Exit 0) = liftIO exitSuccess
catchExit (Exit c) = liftIO $ exitWith $ ExitFailure c
catchExit e        = throwError e

-- TODO: load haskell modules at runtime
globalNamespace :: [(Text, Object)]
globalNamespace = builtins ++ [("time", time)]
