module Owk.Util where

import Data.Attoparsec (parseOnly)
import Data.Attoparsec.Char8 (number)
import Data.Attoparsec.Number (Number)
import Data.Text.Encoding (decodeUtf8)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

showText :: Show a => a -> T.Text
showText = T.pack . show

parseNumber :: T.Text -> Maybe Number
parseNumber t = case parseOnly number $ B.pack $ T.unpack t of
    Right n -> Just n
    _       -> Nothing

lb2text :: BL.ByteString -> T.Text
lb2text bl = decodeUtf8 $ B.concat $ BL.toChunks bl
