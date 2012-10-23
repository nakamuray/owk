{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Owk.Module.Time (time) where

import Data.Time (UTCTime, utcToLocalZonedTime)
import Data.Time.Format (formatTime, parseTime)
import Data.Typeable (cast)
import System.Locale (defaultTimeLocale)

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import Owk.Builtin
import Owk.Type


time :: Object
time = Dict $ H.fromList
    [ ("format", builtin2M format)
    , ("formatLocal", builtin2M formatLocal)
    , ("parse", builtin2M parse)
    ]

format :: Object -> Object -> Owk Object
format (String f) (HaskellData a)
    | Just u <- cast a = return $ String $ T.pack $ formatTime defaultTimeLocale (T.unpack f) (u :: UTCTime)
format _ _ = exception $ String "time.format: type mismatch"

formatLocal :: Object -> Object -> Owk Object
formatLocal (String f) (HaskellData a)
    | Just u <- cast a = do
        z <- liftIO $ utcToLocalZonedTime u
        return $ String $ T.pack $ formatTime defaultTimeLocale (T.unpack f) z
formatLocal _ _ = exception $ String "time.formatLocal: type mismatch"

parse :: Object -> Object -> Owk Object
parse (String f) (String t) =
    case parseTime defaultTimeLocale (T.unpack f) (T.unpack t) of
        Just u  -> return $ HaskellData (u :: UTCTime)
        Nothing -> return $ Undef
parse _ _ = exception $ String "time.parse: type mismatch"
