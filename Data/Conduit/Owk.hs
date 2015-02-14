{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.Owk
  ( owkString
  , owkStringMap
  , owkStringFold
  , owkFile
  , owkFileMap
  -- TODO:
  --, owkFileFold
  , owk
  , owkMap
  , owkFilter
  , owkFold
  ) where

import Data.Conduit

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Cont (callCC)
import Data.Text (Text)

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

owkStringFold :: Text -> Text -> Conduit Object IO Object
owkStringFold script initscript = owkFold "<string>" script initscript

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
            runOwk (withNext $ importProgram fname prog) n

owkMap :: String -> Text -> Conduit Object IO Object
owkMap fname script =
    case parseOwk fname script of
        -- TODO: don't use error, use conduit's error system
        Left e     -> error e
        Right prog -> do
            n <- liftIO $ Namespace.fromList globalNamespace
            (main, s) <- runOwk'' fname prog n
            -- and then, run `main`
            awaitForever $ \obj -> do
                runOwk (withNext $ funcCall main obj) n
                return ()
            h <- liftIO $ Namespace.toHash (Namespace.currentNamepace s)
            case H.lookup "end" h of
                Just end -> do
                    runOwk (withNext $ funcCall end unit) n
                    return ()
                Nothing  -> return ()

owkFilter :: String -> Text -> Conduit Object IO Object
owkFilter fname script =
    case parseOwk fname script of
        -- TODO: don't use error, use conduit's error system
        Left e     -> error e
        Right prog -> do
            n <- liftIO $ Namespace.fromList globalNamespace
            (main, _) <- runOwk'' fname prog n
            -- and then, run `main`
            awaitForever $ \obj -> do
                ret <- runOwk' (withNext $ funcCall main obj) n
                when (bool ret == Bool True) $ yield obj

owkFold :: String -> Text -> Text -> Conduit Object IO Object
owkFold fname script initscript =
    case (parseOwk fname script, parseOwk fname initscript) of
        (Left e, _) -> error e
        (_, Left e) -> error e
        (Right prog, Right proginit) -> do
            n <- liftIO $ Namespace.fromList globalNamespace
            (initval, _) <- runOwk'' fname proginit n
            (main, _) <- runOwk'' fname prog n
            -- and then, run `main`
            go n main initval
          where
            go n main acc = do
                mobj <- await
                case mobj of
                    Nothing  -> yield acc
                    Just obj -> do
                        acc' <- runOwk' (withNext $ f main acc obj) n
                        go n main acc'
            f main acc obj = do
                f' <- withNext $ funcCall main acc
                funcCall f' obj

-- run program and return last expression as a main function
runOwk'' :: String -> AST.Program -> Namespace.Namespace -> OwkPipe (Object, Scope)
runOwk'' fname prog n = flip runOwk' n $ do
    g <- Namespace.extractGlobal <$> askScope
    s <- liftIO $ Namespace.create g
    main <- localScope s $ do
        Namespace.define "__file__" $ String (T.pack fname)
        interpret prog
    return (main, s)

-- Owk with "next" function
withNext :: Owk Object -> Owk Object
withNext o = callCC $ \cont -> do
    g <- Namespace.currentNamepace . Namespace.extractGlobal <$> askScope
    liftIO $ Namespace.insertIO "next" (Function cont) g
    o

-- TODO: load haskell modules at runtime
globalNamespace :: [(Text, Object)]
globalNamespace = builtins ++ [("time", time)]
