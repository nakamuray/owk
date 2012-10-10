{-# LANGUAGE OverloadedStrings #-}
module Owk.IO.Line where

import Data.Conduit

import Control.Applicative ((<$>))

import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

import Owk.IO.Type
import Owk.Type
import Owk.Util

iopipe :: IOPipe
iopipe = IOPipe
    { input = CT.decode CT.utf8 =$= CT.lines =$= CL.map (\line -> Dict
                                                         $ H.fromList
                                                         $ zip (map showText [0..])
                                                         $ String line : map String (T.words line))
    , output = CL.map (\objs -> T.intercalate " " $ map toText objs ++ ["\n"]) =$= CT.encode CT.utf8
    }

toText :: Object -> T.Text
toText (String t) = t
toText obj = let String t = str obj in t
