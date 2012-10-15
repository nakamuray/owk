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

import qualified Data.Text.IO as TI

import qualified Owk.AST as AST
import Owk.Builtin (builtins)
import Owk.Interpreter
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
        Right prog -> conduitOwkProgram prog

owkMain :: String -> Text -> Conduit Object IO [Object]
owkMain fname script =
    case parseOwk fname script of
        -- TODO: don't use error, use conduit's error system
        Left e     -> error e
        Right prog ->
            let prog' = AST.Program [AST.Define "main" $ AST.Function ["$"] $ unProg prog]
            in conduitOwkProgram prog'
  where
    unProg (AST.Program es) = es

conduitOwkProgram :: AST.Program -> Conduit Object IO [Object]
conduitOwkProgram prog = do
    n <- liftIO $ Namespace.fromList builtins
    -- run script and update global namespace,
    runOwk (interpret_ prog `catchError` catchExit) n
    -- search `main` function
    mmain <- liftIO $ Namespace.lookupIO "main" $ Global n
    case mmain of
        -- TODO: don't use error
        Nothing    -> error "no `main` found"
        Just main -> do
            -- and then, run `main`
            awaitForever $ \obj -> do
                runOwk (funcCall main [obj] `catchError` ignoreNext `catchError` catchExit >> return ()) n
                return ()
            mend <- liftIO $ Namespace.lookupIO "end" $ Global n
            case mend of
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
