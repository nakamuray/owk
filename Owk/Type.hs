{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Owk.Type
    ( Owk
    , Object(..)
    , Function
    , ControlFlow(..)
    , Scope(..)
    , Namespace

    , runOwk

    , exit
    , exception
    , next

    , dict
    , list
    , str
    , str'
    , num
    , bool

    , ref
    , readRef
    , writeRef

    , yield
    , await

    , module Control.Monad.Error
    , module Control.Monad.Reader
    , module Control.Monad.Trans
    , module Data.Attoparsec.Number
    ) where

import Data.Conduit

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Monad.Error (Error, ErrorT, MonadError, throwError, runErrorT)
import Control.Monad.Reader (MonadReader, ReaderT(..), runReaderT)
import Control.Monad.Trans (MonadIO, MonadTrans, lift, liftIO)
import Data.Aeson (encode)
import Data.Attoparsec.Number (Number(..))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import Owk.Util


newtype OwkT m a = OwkT (ErrorT ControlFlow (ReaderT Scope m) a)
    deriving (MonadError ControlFlow, MonadReader Scope, MonadIO, Monad, Functor)

instance MonadTrans OwkT where
    lift m = OwkT (lift $ lift m)

type Owk a = OwkT OwkPipe a
type OwkPipe = Pipe Object Object [Object] () IO

data ControlFlow = Return Object | Exit Int | Exception Object | Next
    deriving Show

instance Error ControlFlow

data Scope = Global Namespace | Local Scope Namespace | Builtin
type Namespace = TVar (H.HashMap Text Object)

data Object = Dict (H.HashMap Text Object)
            | List (Vector Object)
            | String !Text
            | Number !Number
            | Bool !Bool
            | Function Scope Function
            | Ref Ref
            | Unit

instance Show Object where
    show (Dict o)     = "Dict " ++ show o
    show (List o)     = "List " ++ show o
    show (String o)   = "String " ++ show o
    show (Number o)   = "Number " ++ show o
    show (Bool o)     = "Bool " ++ show o
    show Unit         = "Unit"
    show (Ref _)      = "Ref"
    show (Function _ _) = "Function"

instance Eq Object where
    Dict o1 == Dict o2 = o1 == o2
    List o1 == List o2 = o1 == o2
    String o1 == String o2 = o1 == o2
    Number o1 == Number o2 = o1 == o2
    Bool o1 == Bool o2 = o1 == o2
    Unit == Unit = True
    -- XXX: how to compare Functions?
    Function _ _ == _ = error "not implemented: how to compare function?"
    _ == Function _ _ = error "not implemented: how to compare function?"
    -- XXX: how to compare Ref without IO?
    Ref _ == Ref _ = error "not implemented: how to comare ref?"
    _ == _ = False

instance Ord Object where
    Ref _ `compare` Ref _ = error "not implemented: how to compare refs?"
    String o1 `compare` String o2 = o1 `compare` o2
    List o1 `compare` List o2 = o1 `compare` o2
    Function _ _ `compare` Function _ _ = error "not implemented: how to compare function?"
    Dict _ `compare` Dict _ = error "not implemented: how to compare dicts?"
    Number o1 `compare` Number o2 = o1 `compare` o2
    Bool o1 `compare` Bool o2 = o1 `compare` o2
    Unit `compare` Unit = EQ

    Ref _ `compare` String _ = GT
    Ref _ `compare` List _ = GT
    Ref _ `compare` Function _ _ = GT
    Ref _ `compare` Dict _ = GT
    Ref _ `compare` Number _ = GT
    Ref _ `compare` Bool _ = GT
    Ref _ `compare` Unit = GT

    String _ `compare` Ref _ = LT
    String _ `compare` List _ = GT
    String _ `compare` Function _ _ = GT
    String _ `compare` Dict _ = GT
    String _ `compare` Number _ = GT
    String _ `compare` Bool _ = GT
    String _ `compare` Unit = GT

    List _ `compare` Ref _ = LT
    List _ `compare` String _ = LT
    List _ `compare` Function _ _ = GT
    List _ `compare` Dict _ = GT
    List _ `compare` Number _ = GT
    List _ `compare` Bool _ = GT
    List _ `compare` Unit = GT

    Function _ _ `compare` Ref _ = LT
    Function _ _ `compare` String _ = LT
    Function _ _ `compare` List _ = LT
    Function _ _ `compare` Dict _ = GT
    Function _ _ `compare` Number _ = GT
    Function _ _ `compare` Bool _ = GT
    Function _ _ `compare` Unit = GT

    Dict _ `compare` Ref _ = LT
    Dict _ `compare` String _ = LT
    Dict _ `compare` Function _ _ = LT
    Dict _ `compare` List _ = LT
    Dict _ `compare` Number _ = GT
    Dict _ `compare` Bool _ = GT
    Dict _ `compare` Unit = GT

    Number _ `compare` Ref _ = LT
    Number _ `compare` String _ = LT
    Number _ `compare` Function _ _ = LT
    Number _ `compare` List _ = LT
    Number _ `compare` Dict _ = LT
    Number _ `compare` Bool _ = GT
    Number _ `compare` Unit = GT

    Bool _ `compare` Ref _ = LT
    Bool _ `compare` String _ = LT
    Bool _ `compare` Function _ _ = LT
    Bool _ `compare` List _ = LT
    Bool _ `compare` Dict _ = LT
    Bool _ `compare` Number _ = LT
    Bool _ `compare` Unit = GT

    Unit `compare` Ref _ = LT
    Unit `compare` String _ = LT
    Unit `compare` Function _ _ = LT
    Unit `compare` List _ = LT
    Unit `compare` Dict _ = LT
    Unit `compare` Number _ = LT
    Unit `compare` Bool _ = LT

type Function = [Object] -> Owk Object
type Ref = TVar Object

runOwk :: Owk () -> Namespace -> Conduit Object IO [Object]
runOwk (OwkT o) n = do
    ret <- runReaderT (runErrorT o) (Global n)
    case ret of
        Left  e -> error $ show e
        Right a -> return a

exit :: Int -> Owk a
exit = throwError . Exit

exception :: Object -> Owk a
exception = throwError . Exception

next :: Owk a
next = throwError Next


dict :: Object -> Object
dict o@(Dict _) = o
dict Unit = Dict H.empty
dict _ = undefined

list :: Object -> Object
list o@(List _) = o
list (Dict h) = List $ V.fromList $ map String $ H.keys h
list Unit = List V.empty
list _ = undefined

str :: Object -> Object
str o@(String _) = o
str (Number n) = String $ showText n
str (Bool True) = String "true"
str (Bool False) = String "false"
str Unit = String ""
str o@(List _) = String $ str' o
str o@(Dict _) = String $ str' o
str o = error $ "str: not implemented: " ++ show o

str' :: Object -> Text
str' (String t) = lb2text $ encode t
str' (Number n) = showText n
str' (Bool True) = "true"
str' (Bool False) = "false"
str' Unit = ""
str' (Ref _) = "ref"
str' (Function _ _) = "function"
str' (List v) = "[" <> T.intercalate ", " (map str' $ V.toList v) <> "]"
str' (Dict h) = "{" <> T.intercalate ", " (map toKV $ H.toList h) <> "}"
  where
    toKV (k, v) = k <> " => " <> str' v

num :: Object -> Object
num (Dict h) = Number $ I $ toInteger $ H.size h
num (List v) = Number $ I $ toInteger $ V.length v
num o@(Number _) = o
num (String t) = Number $ maybe (I 0) id $ parseNumber t
num (Bool True) = Number $ I 1
num (Bool False) = Number $ I 0
num Unit = Number $ I 0
num _ = error "num: not implemented"

bool :: Object -> Object
bool o@(Bool _) = o
bool Unit = Bool False
bool _ = Bool True


ref :: Object -> Owk Ref
ref obj = liftIO $ newTVarIO obj

readRef :: Ref -> Owk Object
readRef t = liftIO $ atomically $ readTVar t

writeRef :: Ref -> Object -> Owk ()
writeRef t obj = liftIO $ atomically $ writeTVar t obj
