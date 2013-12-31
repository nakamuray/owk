{-# LANGUAGE OverloadedStrings #-}
{-
    Labeled Tab-separated Values

    see http://ltsv.org/
-}
module Data.Conduit.Owk.LTSV
    ( toObject
    , fromObjects
    ) where

import Control.Applicative
import Data.Conduit
import Data.Attoparsec.Char8 as A
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import Data.Conduit.Owk.Type
import Owk.Type

toObject :: OwkInput
toObject = CA.conduitParser ltsv =$= awaitForever (yield . snd)

fromObjects :: OwkOutput
fromObjects = CL.map fromObject =$= CT.encode CT.utf8

fromObject :: Object -> Text
fromObject (Tuple os) = T.concat $ map fromObject os
fromObject (Dict h) = (T.intercalate "\t" $ map (\(k, v) -> stripLabel k <> ":" <> stripValue (toText v)) $ H.toList h) <> "\n"
fromObject o = error $ "not a dict: " ++ show o

toText :: Object -> Text
toText (String t) = t
toText obj = let String t = str obj in t

-- XXX: is there any escaping rule for these chars?
stripLabel, stripValue :: Text -> Text
stripLabel = T.filter lbyte
stripValue = T.filter fbyte

--

ltsv :: Parser Object
ltsv = Dict <$> H.map String <$> H.fromList <$> (record <* nl)

record :: Parser [(Text, Text)]
record = field `sepBy1` tab

field :: Parser (Text, Text)
field = do
    l <- label
    char ':'
    v <- fieldValue
    return (l, v)

label :: Parser Text
label = decodeUtf8 <$> takeWhile1 lbyte

fieldValue :: Parser Text
fieldValue = decodeUtf8 <$> A.takeWhile fbyte

tab, nl :: Parser ()
tab = () <$ char '\t'
nl = () <$ (string "\r\n" <|> string "\n")

lbyte, fbyte :: Char -> Bool
lbyte = inClass $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ "_.-"
fbyte = notInClass "\t\r\n" -- %x01-08 / %x0B / %x0C / %x0E-FF
