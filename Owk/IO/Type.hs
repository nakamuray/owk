module Owk.IO.Type
    ( IOPipe(..)
    , Input
    , Output
    ) where

import Data.Conduit
import Data.ByteString.Char8 as B
import Owk.Type

data IOPipe = IOPipe
    { input  :: Conduit B.ByteString IO Object
    , output :: Conduit [Object] IO B.ByteString
    }

type Input = Conduit B.ByteString IO Object
type Output = Conduit [Object] IO B.ByteString
