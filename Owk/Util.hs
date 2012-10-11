module Owk.Util where

import Data.Attoparsec (parseOnly)
import Data.Attoparsec.Char8 (number)
import Data.Attoparsec.Number (Number)

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

showText :: Show a => a -> T.Text
showText = T.pack . show

parseNumber :: T.Text -> Maybe Number
parseNumber t = case parseOnly number $ B.pack $ T.unpack t of
    Right n -> Just n
    _       -> Nothing
