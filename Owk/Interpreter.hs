{-# LANGUAGE OverloadedStrings #-}
module Owk.Interpreter where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Control.Monad.Error (MonadError, catchError)
import Control.Monad.Reader (MonadReader, ask, local)
import Data.Monoid ((<>))

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
funcCall (Type.Ref r) _ = readRef r
funcCall (Type.List v) [Type.Number (I i)]
    | fromInteger i < V.length v = return $ v V.! (fromInteger i)
    | otherwise = return Type.Unit
funcCall (Type.List v) [Type.Number (I i), Type.Number (I j)] = return $ Type.List $ V.slice start count v
  where
    len = V.length v
    start = max 0 $ min len $ fromInteger i
    count = min (len - start) $ fromInteger j
funcCall obj@(Type.List _) [Type.List v] = funcCall obj $ V.toList v
funcCall (Type.List _) _ = exception $ Type.String $ "list only accept 1 or 2 numbers"
funcCall (Type.Dict h) [Type.Dict i] = return $ Type.Dict $ H.union i h
funcCall (Type.Dict _) _ = exception $ Type.String $ "dict only accept 1 other dict"
funcCall obj _ = exception $ Type.String $ "not a function: " <> showText obj

catchReturn :: Owk Object -> Owk Object
catchReturn owk = owk `catchError` catcher
  where
    catcher (Return o) = return o
    catcher e = throwError e
