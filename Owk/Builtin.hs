{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Owk.Builtin where

import Control.Applicative ((<$>))
import Control.Monad ((>=>))
import Control.Monad.Error (catchError)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text.ICU (regex', find)
import System.FilePath ((</>), (<.>), dropFileName, joinPath)

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VI

import Owk.Builtin.Expand (expand)
import Owk.Interpreter
import Owk.Module
import Owk.Type
import Owk.Util
import qualified Owk.Namespace as Namespace

builtins :: [(T.Text, Object)]
builtins =
    [ ("true", Bool True)
    , ("false", Bool False)
    , ("undef", Undef)
    , ("list", builtin1 list)
    , ("str", builtin1 str)
    , ("num", builtin1 num)
    , ("bool", builtin1 bool)
    , ("not", builtin1 __not__)
    , ("ref", Function $ \obj -> Ref <$> ref obj)

    , ("sort", builtin1M sort)
    , ("expand", builtin1M expand)
    , ("putex", builtin1M putex)
    , ("split", builtin2 split)
    , ("length", builtin1M length_)

      -- operators
    , ("+", numop (+))
    , (":", builtin2M __app__)
    , ("/", numop (/))
    , ("*", numop (*))
    , ("__num__", builtin1 num)
    , ("__neg__", builtin1M __neg__)
    , ("-", numop (flip subtract))
    , ("%", numop __mod__)
    , (">", cmpop (>))
    , ("<", cmpop (<))
    , (">=", cmpop (>=))
    , ("<=", cmpop (<=))
    , ("==", cmpop (==))
    , ("/=", cmpop (/=))
    , ("!", builtin1 __not__)
    , ("$$", builtin2 __and__)
    , ("||", builtin2 __or__)
    , ("?", builtin2M __when__)
    , (":=", builtin2M __wref__)
    , ("=~", builtin2M __match__)
    , ("!~", builtin2M __nmatch__)

    -- controls
    , ("if", builtin1M if_)
    , ("then", builtin1M then_)
    , ("else", builtin1M else_)
    , ("case", builtin1M case_)
    , ("for", builtin1M for)
    , ("while", builtin1M while)

    , ("put", builtin1M put)
    , ("get", builtin1M get)
    , ("catch", builtin1M catch_)
    , ("throw", builtin1M throw)
    , ("next", builtin1M $ const next)
    , ("exit", builtin1M exit_)

    , ("import", builtin1M import_')
    ]


-- functions
sort :: Object -> Owk Object
sort (List v) = do
    v' <- liftIO $ do
        vm <- V.thaw v
        VI.sort vm
        V.unsafeFreeze vm
    return $ List v'
sort d@(Dict _) = sort (list d)
sort Undef = sort (list Undef)
sort obj = exception $ String $ "sort: not a List: " <> showText obj

putex :: Object -> Owk Object
putex = expand >=> put

-- TODO: split using regex
split :: Object -> Object -> Object
split sep s
    | String sep' <- str sep
    , String s' <- str s
    = List $ V.fromList $ map String $ T.splitOn sep' s'
split _ _ = error "should not be entered"

length_ :: Object -> Owk Object
length_ (String s) = return $ Number . I $ fromIntegral $ T.length s
length_ (List v) = return $ Number . I $ fromIntegral $ V.length v
length_ (Dict h) = return $ Number . I $ fromIntegral $ length $ H.toList h
length_ obj = exception $ String $ showText obj <> " don't have length"


-- operators
__add__ :: Object -> Object -> Owk Object
__add__ (Number l) (Number r) = return $ Number $ l + r
__add__ l@(Number _) Undef = __add__ l (num Undef)
__add__ Undef r@(Number _) = __add__ (num Undef) r
__add__ (String l) (String r) = return $ String $ l <> r
__add__ l@(String _) Undef = __add__ l (str Undef)
__add__ Undef r@(String _) = __add__ (str Undef) r
__add__ (List l) (List r) = return $ List $ l <> r
__add__ l@(List _) Undef = __add__ l (list Undef)
__add__ Undef r@(List _) = __add__ (list Undef) r
__add__ l r = exception $ String $ "__add__: type mismatch: " <> showText l <> " and " <> showText r

__app__ :: Object -> Object -> Owk Object
__app__ obj arg = funcCall obj arg

__neg__ :: Object -> Owk Object
__neg__ (Number n) = return $ Number $ -n
__neg__ obj = exception $ String $ "not a number: " <> showText obj

__mod__ :: Number -> Number -> Number
__mod__ (I x) (I y) = I $ x `mod` y
__mod__ (D x) y = __mod__ (I $ floor x) y
__mod__ x (D y) = __mod__ x (I $ floor y)

__not__ :: Object -> Object
__not__ obj =
    let Bool b = bool obj
    in Bool (not b)

__and__ :: Object -> Object -> Object
__and__ left right | isTrue left = right
__and__ left _ = left

__or__ :: Object -> Object -> Object
__or__ left _ | isTrue left = left
__or__ _ right = right

__when__ :: Object -> Object -> Owk Object
__when__ b block =
    case bool b of
        Bool True  -> funcCall block unit
        Bool False -> return Undef
        _          -> error "bool should return Bool only"

__wref__ :: Object -> Object -> Owk Object
__wref__ (Ref r) obj = writeRef r obj >> return obj
__wref__ obj _ = exception $ String $ "__wref__: not a Ref: " <> showText obj

__match__ :: Object -> Object -> Owk Object
__match__ (String t) (String pat) =
    case regex' [] pat of
        Left e  -> exception $ String $ "__match__: parse error: " <> showText e
        Right r -> return $ Bool $ isJust $ find r t
__match__ Undef pat = __match__ (str Undef) pat
__match__ (String _) obj = exception $ String $ "__match__: not a String: " <> showText obj
__match__ obj _ = exception $ String $ "__match__: not a String: " <> showText obj

__nmatch__ :: Object -> Object -> Owk Object
__nmatch__ t pat = do
    Bool b <- __match__ t pat
    return $ Bool $ not b


-- controls
-- if .. then .. else
-- use these like::
-- if (true): then {
--   put "true"
-- }: else {
--   put "false"
-- }
if_, then_, else_ :: Function
if_ b = return . Function $ \thenElseBlock -> funcCall thenElseBlock b

then_ block = return . Function $ \elseBlock ->
    return . Function $ \b ->
        case bool b of
            Bool True  -> funcCall block unit
            Bool False -> funcCall elseBlock unit
            _          -> error "bool should return Bool only"

else_ block = return block

case_ :: Function
case_ o = return . Function $ \block -> funcCall block o

for :: Function
for (List v) = return . Function $ \block -> V.foldM (\_ obj -> funcCall block obj) Undef v
for d@(Dict _) = for (list d)
for Undef = for (list Undef)
for _ = exception $ String "for: not implemented"

while :: Function
while cond = return . Function $ \block -> go block Undef
  where
    go block ret = do
        Bool b <- bool <$> funcCall cond unit
        if b
          then funcCall block unit >>= go block
          else return ret

put :: Function
put o = do
    lift $ yield o
    return Undef

get :: Function
get _ = do
    mo <- lift await
    case mo of
        Just o  -> return o
        Nothing -> return Undef

catch_ :: Function
catch_ body = funcCall body unit `catchError` catch'
  where
    catch' (Return obj) = return obj
    catch' e = throwError e

exit_ :: Function
exit_ (Number (I i)) = exit $ fromInteger i
exit_ (Number (D d)) = exit $ error "exit with Double: not implemented" d
exit_ _ = exit 0

import_' :: Object -> Owk Object
import_' (String t) = do
    String myFname <- str <$> Namespace.lookup "__file__"
    let myDirname = dropFileName $ T.unpack myFname
        fname = myDirname </> joinPath (map T.unpack $ T.split (=='.') t) <.> ".owk"
    import_ fname
import_' o = exception $ String $ "import: expect string but " <> showText o


-- helper functions
numop :: (Number -> Number -> Number) -> Object
numop op = builtin2M numop'
  where
    numop' :: Object -> Object -> Owk Object
    numop' (Number l) (Number r) = return $ Number $ l `op` r
    numop' n@(Number _) Undef = numop' n (num Undef)
    numop' Undef n@(Number _) = numop' (num Undef) n
    numop' obj (Number _) = exception $ String $ "not a number: " <> showText obj
    numop' _ (obj) = exception $ String $ "not a number: " <> showText obj

cmpop :: (Object -> Object -> Bool) -> Object
cmpop op = builtin2 $ \left right -> Bool $ op left right

isTrue :: Object -> Bool
isTrue (Bool True) = True
isTrue (Bool False) = False
isTrue obj = isTrue $ bool obj

-- create `Function` from Object to Object function
builtin1 :: (Object -> Object) -> Object
builtin1 f = Function $ return . f

builtin1M :: (Object -> Owk Object) -> Object
builtin1M f = Function f

builtin2 :: (Object -> Object -> Object) -> Object
builtin2 f = Function (\x -> return $ Function (\y -> return $ f x y))

builtin2M :: (Object -> Object -> Owk Object) -> Object
builtin2M f = Function (\x -> return $ Function (\y -> f x y))
