{-# LANGUAGE OverloadedStrings #-}
module Owk.IO.JSON where

import Data.Conduit

import Control.Applicative ((<$>))
import Data.Aeson as A
import Data.Aeson.Parser (json)
import Data.Attoparsec
import System.IO (hPutStrLn, stderr)

import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Owk.IO.Type
import Owk.Type as O
import Owk.Util

iopipe :: IOPipe
iopipe = IOPipe
    { input = CA.conduitParser json =$= awaitForever (yieldObj . fromJSON . snd)
    , output = CL.map (B.concat . BL.toChunks . BL.intercalate " " . map encode)
    }
  where
    yieldObj (A.Error e)            = liftIO $ hPutStrLn stderr e
    yieldObj (A.Success (O.List v)) = V.mapM_ yield v
    yieldObj (A.Success obj)        = yield obj

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
