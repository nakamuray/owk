module Owk.Util where

import Data.Scientific (Scientific)
import Data.Text.Encoding (decodeUtf8)
import Text.Read (readMaybe)
import Text.Trifecta.Delta (Delta)
import Text.Trifecta.Rendering (renderingCaret)
import Text.Trifecta.Result (explain, failed)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

showText :: Show a => a -> T.Text
showText = T.pack . show

parseNumber :: T.Text -> Maybe Scientific
parseNumber t = readMaybe $ T.unpack t

lb2text :: BL.ByteString -> T.Text
lb2text bl = decodeUtf8 $ B.concat $ BL.toChunks bl

prettyError :: Delta -> B.ByteString -> String -> String
prettyError delta line msg =
    let rendering = renderingCaret delta line
        err = failed msg
    in show $ explain rendering err
