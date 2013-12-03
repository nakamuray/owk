{-# LANGUAGE OverloadedStrings #-}
module Owk.Builtin.Expand ( expand ) where

import Text.Trifecta

import Control.Applicative ((<$>), (<*), (*>), (<|>))
import Control.Monad (foldM)
import Data.Monoid ((<>), mempty)
import Data.Text (Text, unpack, snoc)

import Owk.AST (Expression)
import Owk.Interpreter (expr)
import Owk.Parser (expression)
import Owk.Type
import Owk.Util

-- foo = "world"
-- expand "hello ${foo}" # => "hello world"
expand :: Object -> Owk Object
expand (String f) =
    case expandPrep f of
        Failure e  -> exception $ String $ showText e
        Success es -> String <$> foldM go "" es
  where
    go buf (Right c) = return $ buf `snoc` c
    go buf (Left e)  = (buf <>) <$> str'' <$> expr e

    str'' (String t) = t
    str'' o = str' o

expand obj = exception $ String $ "expand: not a String: " <> showText obj

expandPrep :: Text -> Result [Either Expression Char]
expandPrep = parseString p_expand mempty . unpack

p_expand :: Parser [Either Expression Char]
p_expand = many $ try (Right <$> p_escaped) <|> try (Left <$> p_expression) <|> (Right <$> anyChar)

p_escaped :: Parser Char
p_escaped = char '\\' >> char '#'

p_expression :: Parser Expression
p_expression = string "#{" *> expression <* char '}'
