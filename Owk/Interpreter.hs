{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Owk.Interpreter where

import Control.Applicative ((<$>))
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Monad (foldM)
import Control.Monad.Error (Error, ErrorT, MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ReaderT(..), ask, asks, local)
import Control.Monad.Trans (MonadIO)
import Data.Aeson.Types (Value(..))

import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Owk.Namespace as Namespace
import Owk.Util

import Owk.AST as AST
import Owk.Type as Type


interpret :: Program -> Owk Object
interpret (Program es) = foldM (const expr) Type.Unit es

interpret_ :: Program -> Owk ()
interpret_ (Program es) = mapM_ expr es

expr :: Expression -> Owk Object
expr (AST.Function params es) = do
    s <- ask
    return $ Type.Function s $ \args -> do
        mapM (uncurry define) $ zip (params ++ ["_"]) (args ++ repeat Type.Unit)
        foldM (const expr) Type.Unit es
expr (Define name e) = do
    v <- expr e
    define name v
expr (FuncCall efunc eargs) = do
    func <- expr efunc
    args <- mapM expr eargs
    funcCall func args
expr (Variable name) = Namespace.lookup name
expr (AST.String s) = return $ Type.String s
expr (AST.Number n) = return $ Type.Number n
expr (AST.List es) = Type.List . V.fromList <$> mapM expr es
expr (AST.Dict kvs) = Type.Dict . H.fromList <$> mapM (\(k, v) -> expr v >>= \v' -> return (k, v')) kvs
expr AST.Unit = return Type.Unit

funcCall :: Object -> [Object] -> Owk Object
funcCall (Type.Function s f) args = do
    s' <- liftIO $ Namespace.create s
    local (const s') $ catchReturn $ f args
funcCall (Type.Ref ref) _ = readRef ref
funcCall obj _ = exception $ Type.String $ "not a function: " ++. showText obj

catchReturn :: Owk Object -> Owk Object
catchReturn owk = owk `catchError` catcher
  where
    catcher (Return o) = return o
    catcher e = throwError e
