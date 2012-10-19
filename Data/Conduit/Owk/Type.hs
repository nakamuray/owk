module Data.Conduit.Owk.Type
    ( OwkInput
    , OwkOutput
    ) where

import Data.Conduit
import Data.ByteString.Char8 as B
import Owk.Type

type OwkInput = Conduit B.ByteString IO Object
type OwkOutput = Conduit [Object] IO B.ByteString
