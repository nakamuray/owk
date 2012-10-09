{-# LANGUAGE OverloadedStrings #-}
module Owk.IO.JSON where

import Control.Applicative ((<$>))
import Data.Aeson as A
import Data.Aeson.Parser (json)
import Data.Attoparsec
import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Owk.IO.Plugin
import Owk.Type as O
import Owk.Util

ioplugin :: IOPlugin
ioplugin = IOPlugin
    { reader = jsonReader
    , writer = \h objs -> BL.hPutStrLn h $ BL.intercalate " " $ map encode objs
    }

jsonReader h f = go (Right "")
  where
    go (Right buf) = go2 $ parse json buf
    go (Left p) = do
        -- XXX: I want hGet not to wait buffer filled, but couldn't find the way.
        --buf <- B.hGet h bufsize
        buf <- B.hGetLine h
        go2 $ p buf

    go2 e@(Fail _ _ _)  = hPutStrLn stderr $ show e
    go2 (Done buf' j) = do
        case fromJSON j of
            A.Error e -> hPutStrLn stderr e
            A.Success (O.List v) -> V.mapM_ (\o -> f [o]) v
            A.Success obj -> f [obj]
        go (Right buf')
    go2 (Partial p)   = go (Left p)

    bufsize = 4096

instance FromJSON O.Object where
    -- XXX: 何かきれいに書ける方法がありそうな気がしている
    parseJSON (A.Object h) = O.Dict . H.fromList <$> (mapM (\(k, v) -> parseJSON v >>= \v' -> return (k, v')) $ H.toList h)
    parseJSON (A.Array v) = O.List <$> V.mapM parseJSON v
    parseJSON (A.String t) = return $ O.String t
    parseJSON (A.Number n) = return $ O.Number n
    parseJSON (A.Bool b) = return $ O.Bool b
    parseJSON A.Null = return O.Unit

instance ToJSON O.Object where
    toJSON (O.Dict h) = A.Object $ H.map toJSON h
    toJSON (O.List v) = A.Array $ V.map toJSON v
    toJSON (O.String t) = A.String t
    toJSON (O.Number n) = A.Number n
    toJSON (O.Bool b) = A.Bool b
    toJSON (O.Function _ _) = A.String "<Function>"
    toJSON (O.Ref _) = A.String "<Ref>"
    toJSON O.Unit = A.Null
