{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Owk.Builtin where

import Control.Applicative ((<$>))
import Control.Monad ((>=>))
import Control.Monad.Cont (callCC)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Scientific (floatingOrInteger)
import Data.Text.ICU (regex', find)
import System.Exit (ExitCode(..), exitSuccess, exitWith)
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
    , ("+", builtin2M __add__)
    , (":", builtin2M __app__)
    , ("/", numop (/))
    , ("__get__", builtin1M __get__)
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
    , ("&&", builtin2 __and__)
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
    , ("map", builtin2M map_)
    , ("while", builtin1M while)

    , ("put", builtin1M put)
    , ("get", builtin1M get)
    , ("callcc", builtin1M callcc)
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
length_ (String s) = return $ Number  $ fromIntegral $ T.length s
length_ (List v) = return $ Number $ fromIntegral $ V.length v
length_ (Dict h) = return $ Number $ fromIntegral $ length $ H.toList h
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

__mod__ :: Scientific -> Scientific -> Scientific
__mod__ x y = __mod__' (floatingOrInteger x) (floatingOrInteger y)

__mod__' :: Either Double Integer -> Either Double Integer -> Scientific
__mod__' (Right i) (Right j) = fromInteger $ i `mod` j
__mod__' (Left f) y' = __mod__' (Right $ floor f) y'
__mod__' x' (Left f) = __mod__' x' (Right $ floor f)

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


__get__ :: Function
__get__ (List v) = __get__' $ V.toList v
__get__ _ = return Undef

__get__' :: [Object] -> Owk Object
__get__' (Undef:_) = return Undef
__get__' (Dict h:String name:names@(String _:_)) = __get__' $ H.lookupDefault Undef name h : names
__get__' (Dict h:String name:_) = return $ H.lookupDefault Undef name h
__get__' (obj:_) = exception $ String $ "__get__: not a Dict: " <> showText obj
__get__'  [] = error "should not be reached"

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

map_ :: Object -> Object -> Owk Object
map_ f (List v) = List <$> V.mapM (funcCall f) v
map_ f o = map_ f $ list o

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

callcc :: Function
callcc body = callCC $ \cont -> funcCall body (Function cont)

exit_ :: Function
exit_ (Tuple []) = liftIO exitSuccess
exit_ (Number s)
    | s == 0    = liftIO exitSuccess
    | otherwise = liftIO $ exitWith $ ExitFailure $ floor s
exit_ o = exception $ String $ "exit with unknown type: " <> showText o

import_' :: Object -> Owk Object
import_' (String t) = do
    String myFname <- str <$> Namespace.lookup "__file__"
    let myDirname = dropFileName $ T.unpack myFname
        fname = myDirname </> joinPath (map T.unpack $ T.split (=='.') t) <.> ".owk"
    import_ fname
import_' o = exception $ String $ "import: expect string but " <> showText o


-- helper functions
numop :: (Scientific -> Scientific -> Scientific) -> Object
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
