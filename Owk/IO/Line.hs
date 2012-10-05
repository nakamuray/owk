{-# LANGUAGE OverloadedStrings #-}
module Owk.IO.Line where

import Control.Applicative ((<$>))

import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.HashMap.Strict as H

import Owk.Builtin (toText)
import Owk.IO.Plugin
import Owk.Type
import Owk.Util

ioplugin :: IOPlugin
ioplugin = IOPlugin
    { reader = \h f -> do
          mline <- (Just <$> TI.hGetLine h) `catch` (const $ return Nothing)
          case mline of
              Just line -> do
                  let input = Dict
                            $ H.fromList
                            $ zip (map showText [0..])
                            $ String line : map String (T.words line)
                  f [input]
                  reader ioplugin h f
              Nothing -> return ()
    , writer = \h objs -> TI.hPutStrLn h $ T.intercalate " " $ map toText objs
    }
