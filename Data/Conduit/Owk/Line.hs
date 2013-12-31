{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.Owk.Line
    ( toObject
    , fromObjects
    ) where

import Data.Conduit

import Data.Monoid ((<>))

import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

import Data.Conduit.Owk.Type
import Owk.Type
import Owk.Util

toObject :: OwkInput
toObject = CT.decode CT.utf8 =$= CT.lines =$= CL.map (\line -> Dict
                                                     $ H.fromList
                                                     $ zip (map showText [0 :: Int ..])
                                                     $ String line : map String (T.words line))

fromObjects :: OwkOutput
fromObjects = CL.map fromObject =$= CT.encode CT.utf8

fromObject :: Object -> T.Text
fromObject (Tuple os) = T.intercalate " " $ map toText os ++ ["\n"]
fromObject o          = toText o <> "\n"

toText :: Object -> T.Text
toText (String t) = t
toText obj = let String t = str obj in t
