module Owk.Util where

import Data.Scientific (Scientific)
import Data.Text.Encoding (decodeUtf8)
import Text.Read (readMaybe)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

showText :: Show a => a -> T.Text
showText = T.pack . show

parseNumber :: T.Text -> Maybe Scientific
parseNumber t = readMaybe $ T.unpack t

lb2text :: BL.ByteString -> T.Text
lb2text bl = decodeUtf8 $ B.concat $ BL.toChunks bl
