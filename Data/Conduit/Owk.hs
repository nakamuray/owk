{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.Owk
  ( owkString
  , owkStringMain
  , owkFile
  , owkFileMain
  , owk
  , owkMain
  ) where

import Data.Conduit

import Control.Monad.Error (catchError)
import Data.Text (Text)
import System.Exit (ExitCode(..), exitSuccess, exitWith)

import qualified Data.HashMap.Strict as H
import qualified Data.Text.IO as TI

import qualified Owk.AST as AST
import Owk.Builtin (builtins)
import Owk.Interpreter
import Owk.Module
import Owk.Module.Time (time)
import Owk.Parser
import Owk.Type
import qualified Owk.Namespace as Namespace


owkString :: Text -> Conduit Object IO [Object]
owkString script = owk "<string>" script

owkStringMain :: Text -> Conduit Object IO [Object]
owkStringMain script = owkMain "<string>" script

owkFile :: FilePath -> Conduit Object IO [Object]
owkFile fname = do
    script <- liftIO $ TI.readFile fname
    owk fname script

owkFileMain :: FilePath -> Conduit Object IO [Object]
owkFileMain fname = do
    script <- liftIO $ TI.readFile fname
    owkMain fname script

owk :: String -> Text -> Conduit Object IO [Object]
owk fname script =
    case parseOwk fname script of
        -- TODO: don't use error, use conduit's error system
        Left e     -> error e
        Right prog -> conduitOwkProgram fname prog

owkMain :: String -> Text -> Conduit Object IO [Object]
owkMain fname script =
    case parseOwk fname script of
        -- TODO: don't use error, use conduit's error system
        Left e     -> error e
        Right prog ->
            let prog' = AST.Program [AST.Define (AST.PVariable "main") $ AST.Function ["$"] $ unProg prog]
            in conduitOwkProgram fname prog'
  where
    unProg (AST.Program es) = es

conduitOwkProgram :: FilePath -> AST.Program -> Conduit Object IO [Object]
conduitOwkProgram fname prog = do
    n <- liftIO $ Namespace.fromList globalNamespace
    -- run script and get script's namespace,
    Dict h <- runOwk' (importProgram fname prog `catchError` catchExit) n
    -- search `main` function
    case H.lookup "main" h of
        Nothing   -> return ()
        Just main -> do
            -- and then, run `main`
            awaitForever $ \obj -> do
                runOwk (funcCall main [obj] `catchError` ignoreNext `catchError` catchExit >> return ()) n
                return ()
            case H.lookup "end" h of
                Just end -> do
                    runOwk (funcCall end [] `catchError` ignoreNext `catchError` catchExit >> return ()) n
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
