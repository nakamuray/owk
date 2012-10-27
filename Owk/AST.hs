module Owk.AST where

import Data.Attoparsec.Number (Number(..))
import Data.Text (Text)

data Program = Program [Expression]
    deriving (Eq, {- Read, -} Show)

data Expression = Function [(Pattern, [Expression])]
                | Define Pattern Expression
                | FuncCall Expression Expression
                | Variable Text
                | String Text
                | Number Number
                | Tuple [Expression]
                | List [Expression]
                | Dict [(Text, Expression)]
                | Undef
    deriving (Eq, {- Read, -} Show)

data Pattern = PVariable Text
             | PString Text
             | PNumber Number
             | PTuple [Pattern]
             | PList [Pattern]
             | PDict [(Text, Pattern)]
    deriving (Eq, {- Read, -} Show)
