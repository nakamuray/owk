{-# LANGUAGE OverloadedStrings #-}
module Owk.Interpreter where

import Control.Applicative ((<$>))
import Control.Monad (foldM, forM)
import Control.Monad.Reader (MonadReader, ask, local)
import Data.Monoid ((<>))
import Data.Text (Text)

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Owk.Namespace as Namespace
import Owk.Util

import Owk.AST as AST
import Owk.Type as Type


interpret :: Program -> Owk Object
interpret (Program es) = foldM (const expr) Type.Undef es

interpret_ :: Program -> Owk ()
interpret_ (Program es) = mapM_ expr es

expr :: Expression -> Owk Object
expr (AST.Function params es) = do
    s <- ask
    return $ Type.Function $ \args -> do
        ns <- liftIO $ Namespace.fromList $ zip (params ++ ["_"]) (args ++ repeat Type.Undef)
        let scope = Local s ns
        local (const scope) $ foldM (const expr) Type.Undef es
expr (Define p e) = do
    v <- expr e
    case match p v of
        Nothing -> return Type.Undef
        Just ms -> do
            mapM_ (uncurry define) ms
            return v
expr (FuncCall efunc eargs) = do
    func <- expr efunc
    args <- mapM expr eargs
    funcCall func args
expr (Variable name) = Namespace.lookup name
expr (AST.String s) = return $ Type.String s
expr (AST.Number n) = return $ Type.Number n
expr (AST.List es) = Type.List . V.fromList <$> mapM expr es
expr (AST.Dict kvs) = Type.Dict . H.fromList <$> mapM (\(k, v) -> expr v >>= \v' -> return (k, v')) kvs
expr AST.Undef = return Type.Undef

funcCall :: Object -> [Object] -> Owk Object
funcCall (Type.Function f) args = f args
funcCall (Type.Ref r) _ = readRef r
funcCall (Type.List v) [Type.Number (I i)]
    | fromInteger i < V.length v = return $ v V.! (fromInteger i)
    | otherwise = return Type.Undef
funcCall (Type.List v) [Type.Number (I i), Type.Number (I j)] = return $ Type.List $ V.slice start count v
  where
    len = V.length v
    start = max 0 $ min len $ fromInteger i
    count = min (len - start) $ fromInteger j
funcCall obj@(Type.List _) [Type.List v] = funcCall obj $ V.toList v
funcCall (Type.List _) _ = exception $ Type.String $ "list only accept 1 or 2 numbers"
funcCall (Type.Dict h) [Type.Dict i] = return $ Type.Dict $ H.union i h
funcCall (Type.Dict h) [Type.List v]
    | V.length v == 1 =
        let [key] = V.toList v
            Type.String key' = str key
        in return $ H.lookupDefault Type.Undef key' h
    | V.length v == 2 =
        let [key, val] = V.toList v
            Type.String key' = str key
        in return $ Type.Dict $ H.insert key' val h
funcCall (Type.Dict _) _ = exception $ Type.String $ "dict only accept 1 other dict"
funcCall obj _ = exception $ Type.String $ "not a function: " <> showText obj

match :: Pattern -> Object -> Maybe [(Text, Object)]
match (PVariable v) o = Just [(v, o)]
match (PString p) (Type.String t)
    | p == t = Just []
    | otherwise = Nothing
match (PNumber p) (Type.Number n)
    | p == n = Just []
    | otherwise = Nothing
match (PList ps) (Type.List os) = matchList ps $ V.toList os
match (PDict ps) (Type.Dict oh) = matchHash ps oh
match _ _ = Nothing

matchList :: [Pattern] -> [Object] -> Maybe [(Text, Object)]
matchList (p:ps) (o:os) = do
    m <- match p o
    ms <- matchList ps os
    return (m ++ ms)
matchList [] [] = Just []
matchList _ _ = Nothing

matchHash :: [(Text, Pattern)] -> H.HashMap Text Object -> Maybe [(Text, Object)]
matchHash ps oh = do
    mss <- forM ps $ \(t, p) ->
        case H.lookup t oh of
            Just o  -> match p o
            Nothing -> Nothing
    return $ concat mss
