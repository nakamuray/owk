{-# LANGUAGE OverloadedStrings #-}
module Owk.Builtin where

import Prelude hiding (print)
import qualified Prelude

import Control.Applicative ((<$>))
import Control.Monad.Error (catchError)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text.ICU (regex', find)

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VI

import Owk.Interpreter
import Owk.Type
import Owk.Util

builtins :: [(T.Text, Object)]
builtins =
    [ ("true", Bool True)
    , ("false", Bool False)
    , ("undef", Undef)
    , ("list", builtin0 list)
    , ("str", builtin0 str)
    , ("num", builtin0 num)
    , ("bool", builtin0 bool)
    , ("ref", Function Builtin $ \(obj:_) -> Ref <$> ref obj)

    , ("sort", builtin1M sort)

      -- operators
    , ("__add__", builtin2M __add__)
    , ("__app__", builtin __app__)
    , ("__div__", builtin __div__)
    , ("__get__", builtin __get__)
    , ("__mul__", builtin __mul__)
    , ("__num__", builtin0 num)
    , ("__neg__", builtin1M __neg__)
    , ("__sub__", builtin __sub__)
    , ("__mod__", builtin __mod__)
    , ("__gt__", builtin __gt__)
    , ("__lt__", builtin __lt__)
    , ("__ge__", builtin __ge__)
    , ("__le__", builtin __le__)
    , ("__eq__", builtin __eq__)
    , ("__nq__", builtin __nq__)
    , ("__not__", builtin0 __not__)
    , ("__and__", builtin2 __and__)
    , ("__or__", builtin2 __or__)
    , ("__if__", builtin2M __if__)
    , ("__wref__", builtin2M __wref__)
    , ("__match__", builtin2M __match__)
    , ("__nmatch__", builtin2M __nmatch__)

    -- controls
    , ("if", builtin if_)
    , ("then", builtin then_)
    , ("else", builtin else_)
    , ("for", builtin for)
    , ("while", builtin while)

    , ("print", builtin print)
    , ("getobj", builtin getobj)
    , ("catch", builtin catch_)
    , ("throw", builtin1M throw)
    , ("next", builtin $ const next)
    , ("exit", builtin exit_)
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

__app__ :: Function
__app__ (obj:args) = funcCall obj args
__app__ [] = error "should not be reached"

__div__ :: Function
__div__ = numop (/)

__mul__ :: Function
__mul__ = numop (*)

__neg__ :: Object -> Owk Object
__neg__ (Number n) = return $ Number $ -n
__neg__ obj = exception $ String $ "not a number: " <> showText obj

__sub__ :: Function
__sub__ = numop (flip subtract)

__mod__ :: Function
--__mod__ = numop mod
__mod__ = undefined

__gt__ :: Function
__gt__ = mkCmp (>)

__lt__ :: Function
__lt__ = mkCmp (<)

__ge__ :: Function
__ge__ = mkCmp (>=)

__le__ :: Function
__le__ = mkCmp (<=)

__eq__ :: Function
__eq__ = mkCmp (==)

__nq__ :: Function
__nq__ = mkCmp (/=)

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
__get__ (Undef:_) = return Undef
__get__ (Dict h:String name:names@(String _:_)) = __get__ $ H.lookupDefault Undef name h : names
__get__ (Dict h:String name:_) = return $ H.lookupDefault Undef name h
__get__ (obj:_) = exception $ String $ "__get__: not a Dict: " <> showText obj
__get__  [] = error "should not be reached"

__if__ :: Object -> Object -> Owk Object
__if__ b block =
    case bool b of
        Bool True  -> funcCall block []
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
--   print "true"
-- }: else {
--   print "false"
-- }
if_, then_, else_ :: Function
if_ (b:_) = return . Function Builtin $ \(thenElseBlock:_) -> funcCall thenElseBlock [b]
if_ [] = error "should not be reached"

then_ (block:_) = return . Function Builtin $ \(elseBlock:_) ->
    return . Function Builtin $ \(b:_) ->
        case bool b of
            Bool True  -> funcCall block []
            Bool False -> funcCall elseBlock []
            _          -> error "bool should return Bool only"
then_ [] = error "should not be reached"

else_ (block:_) = return block
else_ [] = error "should not be reached"

for :: Function
for (List v:_) = return . Function Builtin $ \(block:_) -> V.foldM (\_ obj -> funcCall block [obj]) Undef v
for (d@(Dict _):args) = for (list d:args)
for (Undef:args) = for (list Undef:args)
for _ = exception $ String "for: not implemented"

while :: Function
while (cond:_) = return . Function Builtin $ \(block:_) -> go block Undef
  where
    go block ret = do
        Bool b <- bool <$> funcCall cond []
        if b
          then funcCall block [] >>= go block
          else return ret
while [] = error "should not be reached"

print :: Function
print args = do
    lift $ yield args
    return $ Bool True

getobj :: Function
getobj _ = do
    mo <- lift await
    case mo of
        Just o  -> return o
        Nothing -> return Undef

catch_ :: Function
catch_ (body:_) = funcCall body [] `catchError` catch'
  where
    catch' (Return obj) = return obj
    catch' e = throwError e
catch_ [] = error "should not be reached"

exit_ :: Function
exit_ (Number (I i):_) = exit $ fromInteger i
exit_ (Number (D d):_) = exit $ error "exit with Double: not implemented" d
exit_ _ = exit 0


-- helper functions
numop :: (Number -> Number -> Number) -> Function
numop op (Number l:Number r:_) = return $ Number $ l `op` r
numop op (n@(Number _):Undef:_) = numop op [n, num Undef]
numop op (Undef:n@(Number _):_) = numop op [num Undef, n]
numop op (n@(Number _):[]) = numop op [n, num Undef]
numop _ (Number _:obj) = exception $ String $ "not a number: " <> showText obj
numop _ (obj:_) = exception $ String $ "not a number: " <> showText obj
numop _ [] = error "should not be reached"

mkOp :: (Object -> Object -> Object) -> Function
mkOp op (left:right:_) = return $ op left right
mkOp op [left] = mkOp op [left, Undef]
mkOp _ [] = error "should not be reached"

mkCmp :: (Object -> Object -> Bool) -> Function
mkCmp op = mkOp $ \left right -> Bool $ op left right

isTrue :: Object -> Bool
isTrue (Bool True) = True
isTrue (Bool False) = False
isTrue obj = isTrue $ bool obj

builtin :: Function -> Object
builtin f = Function Builtin f

-- create `Function` from Object to Object function
builtin0 :: (Object -> Object) -> Object
builtin0 f = Function Builtin $ \(arg:_) -> return $ f arg

builtin1M :: (Object -> Owk Object) -> Object
builtin1M f = Function Builtin $ \(arg:_) -> f arg

builtin2 :: (Object -> Object -> Object) -> Object
builtin2 f = Function Builtin go
  where
    go (arg1:arg2:_) = return $ f arg1 arg2
    go [arg1] = return $ f arg1 Undef
    go [] = error "should no be reached"

builtin2M :: (Object -> Object -> Owk Object) -> Object
builtin2M f = Function Builtin go
  where
    go (arg1:arg2:_) = f arg1 arg2
    go [arg1] = f arg1 Undef
    go [] = error "should no be reached"
