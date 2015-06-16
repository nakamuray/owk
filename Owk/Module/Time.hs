{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Owk.Module.Time (time) where

import Data.Time (UTCTime, utcToLocalZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Typeable (cast)

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import Owk.Builtin
import Owk.Type


time :: Object
time = Dict $ H.fromList
    [ ("format", builtin2M format)
    , ("formatLocal", builtin2M formatLocal)
    , ("parse", builtin2M parse)
    , ("fromPosix", builtin1M fromPosix)
    ]

format :: Object -> Object -> Owk Object
format (String f) (HaskellData a)
    | Just u <- cast a = return $ String $ T.pack $ formatTime defaultTimeLocale (T.unpack f) (u :: UTCTime)
format s n@(Number _) = format s =<< fromPosix n
format _ _ = exception "time.format: type mismatch"

formatLocal :: Object -> Object -> Owk Object
formatLocal (String f) (HaskellData a)
    | Just u <- cast a = do
        z <- liftIO $ utcToLocalZonedTime u
        return $ String $ T.pack $ formatTime defaultTimeLocale (T.unpack f) z
formatLocal s n@(Number _) = formatLocal s =<< fromPosix n
formatLocal _ _ = exception "time.formatLocal: type mismatch"

parse :: Object -> Object -> Owk Object
parse (String f) (String t) =
    case parseTime defaultTimeLocale (T.unpack f) (T.unpack t) of
        Just u  -> return $ HaskellData (u :: UTCTime)
        Nothing -> return $ Undef
parse _ _ = exception "time.parse: type mismatch"

fromPosix :: Object -> Owk Object
fromPosix (Number s) = return $ HaskellData $ posixSecondsToUTCTime $ realToFrac s
fromPosix _ = exception "time.fromPosix: type mismatch"
