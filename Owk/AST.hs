module Owk.AST where

import Data.Attoparsec.Number (Number(..))
import Data.Text (Text)

data Program = Program [Expression]
    deriving (Eq, {- Read, -} Show)

data Expression = Function [Text] [Expression]
                | Define Text Expression
                | FuncCall Expression [Expression]
                | Variable Text
                | String Text
                | Number Number
                | List [Expression]
                | Dict [(Text, Expression)]
                | Undef
    deriving (Eq, {- Read, -} Show)
