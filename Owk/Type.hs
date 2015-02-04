{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Owk.Type
    ( Owk
    , OwkPipe
    , Object(..)
    , Function
    , Scope(..)
    , Namespace

    , runOwk
    , runOwk'

    , exception

    , dict
    , list
    , str
    , str'
    , num
    , bool
    , unit

    , ref
    , readRef
    , writeRef

    , yield
    , await

    , Scientific
    , module Control.Monad.Reader
    , module Control.Monad.Trans
    ) where

import Data.Conduit

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Monad.Cont (ContT(runContT), MonadCont)
import Control.Monad.Reader (MonadReader, ReaderT(..), runReaderT)
import Control.Monad.Trans (MonadIO, MonadTrans, lift, liftIO)
import Data.Aeson (encode)
import Data.IORef (newIORef, writeIORef, readIORef)
import Data.Monoid ((<>))
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vector (Vector)

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import Owk.Util


newtype OwkT m a = OwkT (ContT () (ReaderT Scope m) a)
    deriving (MonadCont, MonadReader Scope, MonadIO, Monad, Functor)

instance MonadTrans OwkT where
    lift m = OwkT (lift $ lift m)

type Owk a = OwkT OwkPipe a
type OwkPipe = ConduitM Object Object IO


data Scope = Global Namespace | Local Scope Namespace
type Namespace = TVar (H.HashMap Text Object)

data Object = Dict (H.HashMap Text Object)
            | List (Vector Object)
            | Tuple [Object]
            | String !Text
            | Number !Scientific
            | Bool !Bool
            | Function Function
            | Ref Ref
            | Undef
            | forall a. (Typeable a, Show a) => HaskellData a

type Function = Object -> Owk Object
type Ref = TVar Object

instance Show Object where
    show (Dict o)     = "Dict " ++ show o
    show (List o)     = "List " ++ show o
    show (Tuple o)    = "Tuple " ++ show o
    show (String o)   = "String " ++ show o
    show (Number o)   = "Number " ++ show o
    show (Bool o)     = "Bool " ++ show o
    show Undef         = "Undef"
    show (Ref _)      = "Ref"
    show (Function _) = "Function"
    show (HaskellData a) = "HaskellData " ++ show a

instance Eq Object where
    Dict o1 == Dict o2 = o1 == o2
    List o1 == List o2 = o1 == o2
    Tuple o1 == Tuple o2 = o1 == o2
    String o1 == String o2 = o1 == o2
    Number o1 == Number o2 = o1 == o2
    Bool o1 == Bool o2 = o1 == o2
    Undef == Undef = True
    -- XXX: how to compare Functions?
    Function _ == _ = error "not implemented: how to compare function?"
    _ == Function _ = error "not implemented: how to compare function?"
    -- XXX: how to compare Ref without IO?
    Ref _ == Ref _ = error "not implemented: how to comare ref?"
    HaskellData _ == HaskellData _ = error "not implemented: how to comare haskell data?"
    _ == _ = False

instance Ord Object where
    HaskellData _ `compare` HaskellData _ = error "not implemented: how to compare haskell data?"
    Ref _ `compare` Ref _ = error "not implemented: how to compare refs?"
    Tuple o1 `compare` Tuple o2 = o1 `compare` o2
    String o1 `compare` String o2 = o1 `compare` o2
    List o1 `compare` List o2 = o1 `compare` o2
    Function _ `compare` Function _ = error "not implemented: how to compare function?"
    Dict _ `compare` Dict _ = error "not implemented: how to compare dicts?"
    Number o1 `compare` Number o2 = o1 `compare` o2
    Bool o1 `compare` Bool o2 = o1 `compare` o2
    Undef `compare` Undef = EQ

    HaskellData _ `compare` Ref _ = GT
    HaskellData _ `compare` Tuple _ = GT
    HaskellData _ `compare` String _ = GT
    HaskellData _ `compare` List _ = GT
    HaskellData _ `compare` Function _ = GT
    HaskellData _ `compare` Dict _ = GT
    HaskellData _ `compare` Number _ = GT
    HaskellData _ `compare` Bool _ = GT
    HaskellData _ `compare` Undef = GT

    Ref _ `compare` HaskellData _ = LT
    Ref _ `compare` Tuple _ = GT
    Ref _ `compare` String _ = GT
    Ref _ `compare` List _ = GT
    Ref _ `compare` Function _ = GT
    Ref _ `compare` Dict _ = GT
    Ref _ `compare` Number _ = GT
    Ref _ `compare` Bool _ = GT
    Ref _ `compare` Undef = GT

    Tuple _ `compare` HaskellData _ = LT
    Tuple _ `compare` Ref _ = LT
    Tuple _ `compare` String _ = GT
    Tuple _ `compare` List _ = GT
    Tuple _ `compare` Function _ = GT
    Tuple _ `compare` Dict _ = GT
    Tuple _ `compare` Number _ = GT
    Tuple _ `compare` Bool _ = GT
    Tuple _ `compare` Undef = GT

    String _ `compare` HaskellData _ = LT
    String _ `compare` Ref _ = LT
    String _ `compare` Tuple _ = LT
    String _ `compare` List _ = GT
    String _ `compare` Function _ = GT
    String _ `compare` Dict _ = GT
    String _ `compare` Number _ = GT
    String _ `compare` Bool _ = GT
    String _ `compare` Undef = GT

    List _ `compare` HaskellData _ = LT
    List _ `compare` Ref _ = LT
    List _ `compare` Tuple _ = LT
    List _ `compare` String _ = LT
    List _ `compare` Function _ = GT
    List _ `compare` Dict _ = GT
    List _ `compare` Number _ = GT
    List _ `compare` Bool _ = GT
    List _ `compare` Undef = GT

    Function _ `compare` HaskellData _ = LT
    Function _ `compare` Ref _ = LT
    Function _ `compare` Tuple _ = LT
    Function _ `compare` String _ = LT
    Function _ `compare` List _ = LT
    Function _ `compare` Dict _ = GT
    Function _ `compare` Number _ = GT
    Function _ `compare` Bool _ = GT
    Function _ `compare` Undef = GT

    Dict _ `compare` HaskellData _ = LT
    Dict _ `compare` Ref _ = LT
    Dict _ `compare` Tuple _ = LT
    Dict _ `compare` String _ = LT
    Dict _ `compare` Function _ = LT
    Dict _ `compare` List _ = LT
    Dict _ `compare` Number _ = GT
    Dict _ `compare` Bool _ = GT
    Dict _ `compare` Undef = GT

    Number _ `compare` HaskellData _ = LT
    Number _ `compare` Ref _ = LT
    Number _ `compare` Tuple _ = LT
    Number _ `compare` String _ = LT
    Number _ `compare` Function _ = LT
    Number _ `compare` List _ = LT
    Number _ `compare` Dict _ = LT
    Number _ `compare` Bool _ = GT
    Number _ `compare` Undef = GT

    Bool _ `compare` HaskellData _ = LT
    Bool _ `compare` Ref _ = LT
    Bool _ `compare` Tuple _ = LT
    Bool _ `compare` String _ = LT
    Bool _ `compare` Function _ = LT
    Bool _ `compare` List _ = LT
    Bool _ `compare` Dict _ = LT
    Bool _ `compare` Number _ = LT
    Bool _ `compare` Undef = GT

    Undef `compare` HaskellData _ = LT
    Undef `compare` Ref _ = LT
    Undef `compare` Tuple _ = LT
    Undef `compare` String _ = LT
    Undef `compare` Function _ = LT
    Undef `compare` List _ = LT
    Undef `compare` Dict _ = LT
    Undef `compare` Number _ = LT
    Undef `compare` Bool _ = LT

runOwk :: Owk a -> Namespace -> Conduit Object IO Object
runOwk o n = runOwk' o n >> return ()

runOwk' :: Owk a -> Namespace -> OwkPipe a
runOwk' (OwkT o) n = do
    -- FIXME: don't use IORef
    ref <- liftIO $ newIORef undefined
    runReaderT (runContT o (\a -> liftIO $ writeIORef ref a)) (Global n)
    liftIO $ readIORef ref

exception :: Object -> Owk a
exception = error . show


dict :: Object -> Object
dict o@(Dict _) = o
dict Undef = Dict H.empty
dict _ = undefined

list :: Object -> Object
list o@(List _) = o
list (Dict h) = List $ V.fromList $ map String $ H.keys h
list Undef = List V.empty
list (String t) = List $ V.fromList $ map (String . T.singleton) $ T.unpack t
list _ = undefined

str :: Object -> Object
str o@(String _) = o
str (Number s) =
    -- XXX: to avoid ambiguous type warning, declare type explicitly
    case floatingOrInteger s :: Either Double Integer of
        Right i -> String $ showText i
        Left  f -> String $ showText f
str (Bool True) = String "true"
str (Bool False) = String "false"
str Undef = String ""
str o@(List _) = String $ str' o
str o@(Dict _) = String $ str' o
str o@(HaskellData _) = String $ str' o
str o = error $ "str: not implemented: " ++ show o

str' :: Object -> Text
str' (String t) = lb2text $ encode t
str' (Number s) =
    -- XXX: to avoid ambiguous type warning, declare type explicitly
    case floatingOrInteger s :: Either Double Integer of
        Right i -> showText i
        Left  f -> showText f
str' (Bool True) = "true"
str' (Bool False) = "false"
str' Undef = ""
str' (Ref _) = "ref"
str' (Function _) = "function"
str' (Tuple os) = "(" <> T.intercalate ", " (map str' os) <> ")"
str' (List v) = "[" <> T.intercalate ", " (map str' $ V.toList v) <> "]"
str' (Dict h) = "{" <> T.intercalate ", " (map toKV $ H.toList h) <> "}"
  where
    toKV (k, v) = k <> " => " <> str' v
str' (HaskellData a) = showText a

num :: Object -> Object
num (Dict h) = Number $ fromIntegral $ H.size h
num (List v) = Number $ fromIntegral $ V.length v
num o@(Number _) = o
num (String t) = Number $ maybe 0 id $ parseNumber t
num (Bool True) = Number 1
num (Bool False) = Number 0
num Undef = Number 0
num _ = error "num: not implemented"

bool :: Object -> Object
bool o@(Bool _) = o
bool Undef = Bool False
bool _ = Bool True

unit :: Object
unit = Tuple []


ref :: Object -> Owk Ref
ref obj = liftIO $ newTVarIO obj

readRef :: Ref -> Owk Object
readRef t = liftIO $ atomically $ readTVar t

writeRef :: Ref -> Object -> Owk ()
writeRef t obj = liftIO $ atomically $ writeTVar t obj
