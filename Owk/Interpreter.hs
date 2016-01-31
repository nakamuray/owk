{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Owk.Interpreter where

import Control.Applicative ((<$>))
import Control.Monad (foldM, forM)
import Data.Monoid ((<>))
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)

import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Owk.Namespace as Namespace

import Owk.AST as AST
import Owk.Type as Type


interpret :: Program -> Owk Object
interpret (Program es) = foldM (const expr) Type.Undef es

interpret_ :: Program -> Owk ()
interpret_ (Program es) = mapM_ expr es

expr :: Expression -> Owk Object
expr (AST.Function pbs) = do
    s <- askScope
    return $ Type.Function $ func s (exprBodies pbs)
  where
    func _ [] _ = return $ Type.Undef
    func s ((p, mguard, body) : pbs') v = do
        case match p v of
            Nothing -> func s pbs' v
            Just ms -> do
                ns <- liftIO $ Namespace.fromList ms
                let scope = Local s ns
                case mguard of
                    Nothing    -> func' scope body
                    Just guard -> do
                        gsuccess <- bool <$> localScope scope guard
                        case gsuccess of
                            Type.Bool True -> func' scope body
                            _              -> func s pbs' v
    func' _ [] = return Type.Undef
    func' scope [o] = localScope scope o
    func' scope body = localScope scope $ foldM (const id) Type.Undef body
    exprBodies [] = []
    exprBodies ((p, mguard, body) : pbs'') = (p, expr <$> mguard, map expr body) : exprBodies pbs''
expr (Define p e) = do
    v <- expr e
    case match p v of
        Nothing -> return Type.Undef
        Just ms -> do
            mapM_ (uncurry define) ms
            return v
expr (FuncCall loc efunc earg) =
    let ofunc = expr efunc
        oarg = expr earg
    in do
        func <- ofunc
        arg <- oarg
        setLocation loc
        funcCall func arg
expr (Variable name) = Namespace.lookup name
expr (AST.String s) = return $ Type.String s
expr (AST.Number n) = return $ Type.Number n
expr (AST.Tuple es) =
    let es' = map expr es
    in Type.Tuple <$> sequence es'
expr (AST.List es) =
    let es' = map expr es
    in Type.List . V.fromList <$> sequence es'
expr (AST.Dict kvs) =
    let kvs' = map (expr <$>) kvs
    in Type.Dict . H.fromList <$> mapM (\(k, v) -> v >>= \v' -> return (k, v')) kvs'
expr AST.Undef = return Type.Undef

funcCall :: Object -> Object -> Owk Object
funcCall (Type.Function f) arg = f arg
funcCall (Type.Ref r) _ = readRef r
funcCall (Type.List v) (Type.Number s)
    | Just i <- toBoundedInteger s = return $ v !!! i
funcCall (Type.List v) (Type.List args)
    | [Type.Number s] <- V.toList args,
      Just i <- toBoundedInteger s = return $ v !!! i
    | [Type.Number s, Type.Number t] <- V.toList args,
      (Just i, Just j) <- (toBoundedInteger s, toBoundedInteger t) =
        let len = V.length v
            i' = if i >= 0 then i else i + len
            start = max 0 $ min len i'
            count = min (len - start) j
        in return $ Type.List $ V.slice start count v
funcCall (Type.List _) _ = exception "list only accept 1 or 2 numbers"
funcCall (Type.Dict h) (Type.Dict i) = return $ Type.Dict $ H.union i h
funcCall (Type.Dict h) (Type.List v)
    | V.length v == 1 =
        let [key] = V.toList v
            Type.String key' = str key
        in return $ H.lookupDefault Type.Undef key' h
    | V.length v == 2 =
        let [key, val] = V.toList v
            Type.String key' = str key
        in return $ Type.Dict $ H.insert key' val h
funcCall (Type.Dict h) (Type.String k) = return $ H.lookupDefault Type.Undef k h
funcCall (Type.Dict _) o = exception $ "dict accept other dict or string, but :" <> show o
funcCall obj _ = exception $ "not a callable: " <> show obj

match :: Pattern -> Object -> Maybe [(Text, Object)]
match (PVariable v Nothing) o = Just [(v, o)]
match (PVariable v (Just p)) o = do
    vos <- match p o
    return $ (v, o) : vos
match (PString p) (Type.String t)
    | p == t = Just []
    | otherwise = Nothing
match (PNumber p) (Type.Number n)
    | p == n = Just []
    | otherwise = Nothing
match (PTuple ps) (Type.Tuple os) = matchList ps os
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

(!!!) :: V.Vector Object -> Int -> Object
(!!!) v i | i >= 0    = if i < V.length v then v V.! i else Type.Undef
          | otherwise = if negate i <= V.length v then v V.! (V.length v + i) else Type.Undef
