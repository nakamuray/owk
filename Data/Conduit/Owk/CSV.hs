{-# LANGUAGE OverloadedStrings #-}

module Data.Conduit.Owk.CSV
    ( toObject
    , fromObjects
    ) where

import Data.Conduit
import Data.Text (Text)

import qualified Data.Vector as V
import qualified Data.Conduit.List as CL
import qualified Data.CSV.Conduit as CC

import Data.Conduit.Owk.Type
import Owk.Type

toObject :: OwkInput
toObject = CC.intoCSV CC.defCSVSettings =$= CL.map (List . V.fromList . map String)

fromObjects :: OwkOutput
fromObjects = CL.map fromObject =$= CC.fromCSV CC.defCSVSettings

fromObject :: Object -> [Text]
fromObject (List v) = map toText $ V.toList v
fromObject (Tuple os) = map toText os
fromObject o = error $ "not a list|tuple: " ++ show o

toText :: Object -> Text
toText (String t) = t
toText obj = let String t = str obj in t
