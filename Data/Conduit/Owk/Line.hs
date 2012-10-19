{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.Owk.Line
    ( toObject
    , fromObjects
    ) where

import Data.Conduit

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
fromObjects = CL.map (\objs -> T.intercalate " " $ map toText objs ++ ["\n"]) =$= CT.encode CT.utf8

toText :: Object -> T.Text
toText (String t) = t
toText obj = let String t = str obj in t
