{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.Owk.JSON
    ( toObject
    , fromObjects
    , fromObjectsPretty
    ) where

import Data.Conduit

import Control.Applicative ((<$), (<$>), (<|>))
import Data.Aeson as A
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Attoparsec.ByteString.Char8 (endOfInput, skipSpace)
import System.IO (hPutStrLn, stderr)

import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

import Data.Conduit.Owk.Type
import Owk.Type as O
import Owk.Util

toObject :: OwkInput
toObject = CA.conduitParser jsonOrEmpty =$= awaitForever (yieldObj . fmap fromJSON . snd)
  where
    jsonOrEmpty = (Just <$> A.json) <|> (Nothing <$ (skipSpace >> endOfInput))

    yieldObj (Just (A.Error e))            = liftIO $ hPutStrLn stderr e
    yieldObj (Just (A.Success (O.List v))) = V.mapM_ yield v
    yieldObj (Just (A.Success obj))        = yield obj
    yieldObj Nothing                       = return ()

fromObjects :: OwkOutput
fromObjects = fromObjects' encode

fromObjectsPretty :: OwkOutput
fromObjectsPretty = fromObjects' encodePretty

fromObjects' :: (O.Object -> BL.ByteString) -> OwkOutput
fromObjects' encoder = CL.map $ \obj ->
    flip B.append "\n" $ case obj of
        Tuple os -> B.concat $ BL.toChunks $ BL.intercalate " " $ map encoder os
        o        -> BL.toStrict $ encoder o

instance FromJSON O.Object where
    -- XXX: 何かきれいに書ける方法がありそうな気がしている
    parseJSON (A.Object h) = O.Dict . H.fromList <$> (mapM (\(k, v) -> parseJSON v >>= \v' -> return (k, v')) $ H.toList h)
    parseJSON (A.Array v) = O.List <$> V.mapM parseJSON v
    parseJSON (A.String t) = return $ O.String t
    parseJSON (A.Number n) = return $ O.Number n
    parseJSON (A.Bool b) = return $ O.Bool b
    parseJSON A.Null = return O.Undef

instance ToJSON O.Object where
    toJSON (O.Dict h) = A.Object $ H.map toJSON h
    toJSON (O.List v) = A.Array $ V.map toJSON v
    toJSON (O.Tuple os) = A.Array . V.fromList $ map toJSON os
    toJSON (O.String t) = A.String t
    toJSON (O.Number n) = A.Number n
    toJSON (O.Bool b) = A.Bool b
    toJSON (O.Function _) = A.String "<Function>"
    toJSON (O.Ref _) = A.String "<Ref>"
    toJSON O.Undef = A.Null
    toJSON (O.HaskellData a) = A.String (showText a)
