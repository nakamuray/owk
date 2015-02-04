{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.Owk.ApacheLog ( toObject ) where

import Control.Applicative
import Data.Conduit
import Data.Attoparsec.Char8 as A

import Data.Monoid (mconcat)
import Data.Text.Encoding (decodeUtf8)

import qualified Data.ByteString.Char8 as B
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.HashMap.Strict as H

import Data.Conduit.Owk.Type
import Owk.Type

toObject :: OwkInput
toObject = CA.conduitParser apacheLog =$= awaitForever (yield . snd)

apacheLog :: Parser Object
apacheLog = do
    ip <- val
    space
    identity <- val
    space
    user <- val
    space
    date <- datetime
    space
    -- TODO: split req into method, path, version
    req <- quoted
    let (method, req') = B.span (/= ' ') req
        (req'', version) = B.spanEnd (/= ' ') req'
        path = B.tail $ B.init req''
    space
    status <- decimal
    space
    size <- mdecimal
    space
    referer <- quoted
    space
    ua <- quoted
    -- ignore unknown fields
    takeTill (== '\n')
    char '\n'

    return $ Dict $ H.fromList
        [ ("host", b2s ip)
        , ("identity", b2s' identity)
        , ("user", b2s' user)
        , ("time", b2s date)
        , ("req", b2s req)
        , ("method", b2s method)
        , ("path", b2s path)
        , ("version", b2s version)
        , ("status", n status)
        , ("size", n' size)
        , ("referer", b2s' referer)
        , ("ua", b2s' ua)
        ]

  where
    b2s = String . decodeUtf8
    b2s' b | b == "-" = Undef
           | otherwise = b2s b
    n = Number . fromInteger
    n' (Just i) = n i
    n' Nothing = Undef

mdecimal :: Parser (Maybe Integer)
mdecimal = (Just <$> try decimal) <|> (char '-' >> return Nothing)

val :: Parser B.ByteString
val = takeTill (== ' ')

quoted :: Parser B.ByteString
quoted = char '"' *> inner <* char '"'
  where
    inner = mconcat <$> many (A.takeWhile1 noQ <|> escaped)
    noQ '"' = False
    noQ '\\' = False
    noQ _ = True
    escaped = char '\\' >> (string "\"" <|> h)
    -- XXX: it may contain any bytes, which break UTF-8 decoding
    h = do
        c <- anyChar
        return $ '\\' `B.cons` (c `B.cons` B.empty)

datetime :: Parser B.ByteString
datetime = do
    char '['
    d <- takeWhile1 isDigit
    char '/'
    m <- month
    char '/'
    y <- takeTill (== ':')
    char ':'
    hms <- takeTill (== ' ')
    char ' '
    tz <- takeTill (== ']')
    char ']'
    return $ mconcat [y, "/", m, "/", d, " ", hms, " ", tz]

month :: Parser B.ByteString
month = do
    m <- A.take 3
    return $ case m of
        "Jan" -> "1"
        "Fed" -> "2"
        "May" -> "3"
        "Jun" -> "6"
        "Jul" -> "7"
        "Aug" -> "8"
        "Sep" -> "9"
        "Oct" -> "10"
        "Nov" -> "11"
        "Dec" -> "12"
        _     -> "unknown"
