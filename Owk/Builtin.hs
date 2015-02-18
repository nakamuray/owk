{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Owk.Builtin where

import Data.Conduit
import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Cont (callCC)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Scientific (floatingOrInteger)
import Data.Text.ICU (regex', find)
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import System.FilePath ((</>), (<.>), dropFileName, joinPath)

import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VI

import Owk.Builtin.Expand (expand)
import Owk.Interpreter
import Owk.Module
import Owk.Type
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
    , ("split", builtin2 split)
    , ("length", builtin1M length_)

      -- operators
    , ("+", builtin2M __add__)
    , ("$", builtin2M __app__)
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
    , ("while", builtin1M while)

    -- stream functions
    , (">>", builtin2 __comp__)
    , ("map", builtin2M map_)
    , ("m", builtin2M map_)
    , ("filter", builtin2M filter_)
    , ("f", builtin2M filter_)
    , ("fold", builtin3M fold_)
    , ("reduce", builtin2M reduce_)
    , ("r", builtin2M reduce_)
    , ("consume", builtin1M consume_)
    , ("drop", builtin2 drop_)
    , ("take", builtin2 take_)
    , ("zip", builtin2 zip_)
    , ("counter", builtin1 counter)
    , ("tail", builtin2 tail_)
    , ("stream", builtin1 stream)

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
sort obj = exception $ "sort: not a List: " <> show obj

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
length_ obj = exception $ show obj <> " don't have length"


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
__add__ l r = exception $ "__add__: type mismatch: " <> show l <> " and " <> show r

__app__ :: Object -> Object -> Owk Object
__app__ obj arg = funcCall obj arg

__neg__ :: Object -> Owk Object
__neg__ (Number n) = return $ Number $ -n
__neg__ obj = exception $ "not a number: " <> show obj

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
__get__' (obj:_) = exception $ "__get__: not a Dict: " <> show obj
__get__'  [] = error "should not be reached"

__when__ :: Object -> Object -> Owk Object
__when__ b block =
    case bool b of
        Bool True  -> funcCall block unit
        Bool False -> return Undef
        _          -> error "bool should return Bool only"

__wref__ :: Object -> Object -> Owk Object
__wref__ (Ref r) obj = writeRef r obj >> return obj
__wref__ obj _ = exception $ "__wref__: not a Ref: " <> show obj

__match__ :: Object -> Object -> Owk Object
__match__ (String t) (String pat) =
    case regex' [] pat of
        Left e  -> exception $ "__match__: parse error: " <> show e
        Right r -> return $ Bool $ isJust $ find r t
__match__ Undef pat = __match__ (str Undef) pat
__match__ (String _) obj = exception $ "__match__: not a String: " <> show obj
__match__ obj _ = exception $ "__match__: not a String: " <> show obj

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
for _ = exception "for: not implemented"

map_ :: Object -> Object -> Owk Object
map_ f (Stream s) = return $ Stream (s $= CL.mapM (funcCall f))
map_ f (List v) = List <$> V.mapM (funcCall f) v
map_ f o = map_ f $ list o

filter_ :: Object -> Object -> Owk Object
filter_ f (List v) = List <$> V.filterM (\o -> bool' <$> funcCall f o) v
filter_ f (Stream s) =
    return $ Stream $ s =$ awaitForever (\o -> do
        ret <- lift $ funcCall f o
        when (bool' ret) $ yield o)
filter_ f o = filter_ f $ list o

fold_ :: Object -> Object -> Object -> Owk Object
fold_ f i (List v) = V.foldM' (\a -> \b -> funcCall f a >>= \f' -> funcCall f' b) i v
fold_ f i (Stream s) = return $ Stream $ s =$ go i
  where
    go acc = do
        mobj <- await
        case mobj of
            Nothing  -> yield acc
            Just obj -> do
                acc' <- lift $ funcCall f acc >>= \f' -> funcCall f' obj
                go acc'
fold_ f i o = fold_ f i $ list o

reduce_ :: Object -> Object -> Owk Object
reduce_ (Tuple [f, i]) o = fold_ f i o
reduce_ f o = fold_ f Undef o

consume_ :: Function
consume_ (Stream s) = do
    l <- s $$ CL.consume
    return $ List $ V.fromList l
consume_ o = return $ list o

drop_ :: Object -> Object -> Object
drop_ n (List v) = List $ V.drop (_toInt n) v
drop_ n (Stream s) = Stream $ s $= (CL.drop (_toInt n) >> CL.map id)
drop_ n o = drop_ n $ list o

take_ :: Object -> Object -> Object
take_ n (List v) = List $ V.take (_toInt n) v
take_ n (Stream s) = Stream $ s $= CL.isolate (_toInt n)
take_ n o = take_ n $ list o

zip_ :: Object -> Object -> Object
zip_ (Stream s1) (Stream s2) = Stream $ sequenceSources [s1, s2] $= CL.map Tuple
zip_ s@(Stream _) o = zip_ s (stream o)
zip_ o s@(Stream _) = zip_ (stream o) s
zip_ (List v1) (List v2) = List $ V.map (\(o1, o2) -> Tuple [o1, o2]) $ V.zip v1 v2
zip_ o1 o2 = zip_ (list o1) (list o2)

counter :: Object -> Object
counter n = Stream $ CL.sourceList $ map (Number . fromInteger) [(_toInt n)..]

tail_ :: Object -> Object -> Object
tail_ n (List v) = List $ V.drop (V.length v - _toInt n) v
tail_ n (Stream s) = Stream $ s $= go []
  where
    i = _toInt n
    go acc = do
        mo <- await
        case mo of
            Nothing -> CL.sourceList $ reverse acc
            Just o  -> go $ take i $ o:acc
tail_ n o = tail_ n (list o)

_toInt :: Integral a => Object -> a
_toInt o = let Number i = num o in floor i

stream :: Object -> Object
stream s@(Stream _) = s
stream (List v) = Stream $ CL.sourceList $ V.toList v
stream o = stream $ list o

__comp__ :: Object -> Object -> Object
__comp__ f g = Function $ \o -> funcCall f o >>= funcCall g

while :: Function
while cond = return . Function $ \block -> go block Undef
  where
    go block ret = do
        Bool b <- bool <$> funcCall cond unit
        if b
          then funcCall block unit >>= go block
          else return ret

callcc :: Function
callcc body = callCC $ \cont -> funcCall body (Function cont)

exit_ :: Function
exit_ (Tuple []) = liftIO exitSuccess
exit_ (Number s)
    | s == 0    = liftIO exitSuccess
    | otherwise = liftIO $ exitWith $ ExitFailure $ floor s
exit_ o = exception $ "exit with unknown type: " <> show o

import_' :: Object -> Owk Object
import_' (String t) = do
    String myFname <- str <$> Namespace.lookup "__file__"
    let myDirname = dropFileName $ T.unpack myFname
        fname = myDirname </> joinPath (map T.unpack $ T.split (=='.') t) <.> ".owk"
    import_ fname
import_' o = exception $ "import: expect string but " <> show o


-- helper functions
numop :: (Scientific -> Scientific -> Scientific) -> Object
numop op = builtin2M numop'
  where
    numop' :: Object -> Object -> Owk Object
    numop' (Number l) (Number r) = return $ Number $ l `op` r
    numop' n@(Number _) Undef = numop' n (num Undef)
    numop' Undef n@(Number _) = numop' (num Undef) n
    numop' obj (Number _) = exception $ "not a number: " <> show obj
    numop' _ (obj) = exception $ "not a number: " <> show obj

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

builtin3M :: (Object -> Object -> Object -> Owk Object) -> Object
builtin3M f = Function (\x -> return $ Function (\y ->  return $ Function (\z -> f x y z)))
