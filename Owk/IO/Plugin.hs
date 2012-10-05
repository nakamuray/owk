module Owk.IO.Plugin
    ( IOPlugin(..)
    , IOReader
    , IOWriter
    ) where

import System.IO (Handle)

import Owk.Type

data IOPlugin = IOPlugin
    { reader :: IOReader
    , writer :: IOWriter
    }

type IOReader = Handle -> ([Object] -> IO ()) -> IO ()
type IOWriter = Handle -> [Object] -> IO ()
