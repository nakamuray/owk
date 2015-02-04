module Owk.AST where

import Data.Scientific (Scientific)
import Data.Text (Text)

data Program = Program [Expression]
    deriving (Eq, {- Read, -} Show)

data Expression = Function [(Pattern, Maybe Expression, [Expression])]
                | Define Pattern Expression
                | FuncCall Expression Expression
                | Variable Text
                | String Text
                | Number Scientific
                | Tuple [Expression]
                | List [Expression]
                | Dict [(Text, Expression)]
                | Undef
    deriving (Eq, {- Read, -} Show)

data Pattern = PVariable Text (Maybe Pattern)
             | PString Text
             | PNumber Scientific
             | PTuple [Pattern]
             | PList [Pattern]
             | PDict [(Text, Pattern)]
    deriving (Eq, {- Read, -} Show)
