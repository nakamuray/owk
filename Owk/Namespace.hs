{-# LANGUAGE OverloadedStrings #-}
module Owk.Namespace
  ( module Owk.Type

  , fromList
  , create
  , define
  , lookup
  , lookupIO
  , insertIO
  ) where

import Prelude hiding (lookup)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar, writeTVar)
import Control.Monad.Reader (ask, asks)
import Data.Maybe (isJust)
import Data.Text (Text)

import qualified Data.HashMap.Strict as H

import Owk.Type (Namespace, Scope(..))
import Owk.Util

import Owk.Type as Type


fromList :: [(Text, Object)] -> IO Namespace
fromList l = newTVarIO $ H.fromList l

create :: Scope -> IO Scope
create parent = do
  ns <- newTVarIO H.empty
  return $ Local parent ns

define :: Text -> Object -> Owk Object
define n v = do
    ns <- asks currentNamepace
    ret <- liftIO $ atomically $ do
        h <- readTVar ns
        if isJust $ H.lookup n h
          then return Nothing
          else do
            writeTVar ns $ H.insert n v h
            return $ Just v
    case ret of
        Just v'  -> return v'
        Nothing -> exception $ Type.String $ "name `" ++. n ++. "` is already defined"

currentNamepace :: Scope -> Namespace
currentNamepace (Global n) = n
currentNamepace (Local _ n) = n
currentNamepace Builtin = error "built-in functions should not use namespace"

parentScope :: Scope -> Maybe Scope
parentScope (Global _)  = Nothing
parentScope (Local s _) = Just s
parentScope Builtin  = Nothing

lookup :: Text -> Owk Object
lookup n = do
    s <- ask
    mo <- liftIO $ atomically $ lookup' n s
    case mo of
        Just o  -> return o
        Nothing -> return Type.Unit

lookupIO :: Text -> Scope -> IO (Maybe Object)
lookupIO n s = atomically $ lookup' n s

lookup' :: Text -> Scope -> STM (Maybe Object)
lookup' n s = do
    let ns = currentNamepace s
    h <- readTVar ns
    case (H.lookup n h, parentScope s) of
        (Just o, _)        -> return $ Just o
        (Nothing, Just s') -> lookup' n s'
        _                  -> return Nothing

insertIO :: Text -> Object -> Namespace -> IO ()
insertIO n v ns = atomically $ do
    h <- readTVar ns
    writeTVar ns $ H.insert n v h
