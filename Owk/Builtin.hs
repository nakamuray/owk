{-# LANGUAGE OverloadedStrings #-}
module Owk.Builtin where

import Prelude hiding (print)
import qualified Prelude

import Control.Applicative ((<$>))
import Control.Monad.Error (catchError, throwError)
import Data.Attoparsec.Number (Number(..))
import Data.Maybe (isJust)
import Data.Text.ICU (regex', find)

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VI

import Owk.Interpreter
import Owk.Type
import Owk.Util

builtins :: [(T.Text, Object)]
builtins =
    [ ("true", Bool True)
    , ("false", Bool False)
    , ("unit", Unit)
    , ("list", builtin0 list)
    , ("str", builtin0 str)
    , ("num", builtin0 num)
    , ("bool", builtin0 bool)
    , ("ref", Function Builtin $ \(obj:_) -> Ref <$> ref obj)

    , ("sort", builtin sort)

      -- operators
    , ("__add__", builtin __add__)
    , ("__app__", builtin __app__)
    , ("__div__", builtin __div__)
    , ("__get__", builtin __get__)
    , ("__mul__", builtin __mul__)
    , ("__num__", builtin __num__)
    , ("__neg__", builtin __neg__)
    , ("__sub__", builtin __sub__)
    , ("__mod__", builtin __mod__)
    , ("__gt__", builtin __gt__)
    , ("__lt__", builtin __lt__)
    , ("__ge__", builtin __ge__)
    , ("__le__", builtin __le__)
    , ("__eq__", builtin __eq__)
    , ("__nq__", builtin __nq__)
    , ("__not__", builtin __not__)
    , ("__and__", builtin __and__)
    , ("__or__", builtin __or__)
    , ("__if__", builtin __if__)
    , ("__wref__", builtin __wref__)
    , ("__match__", builtin __match__)
    , ("__nmatch__", builtin __nmatch__)

    -- controls
    , ("if", builtin if_)
    , ("for", builtin for)
    , ("while", builtin while)

    , ("return", builtin return_)
    , ("print", builtin print)
    , ("getobj", builtin getobj)
    , ("catch", builtin catch_)
    , ("throw", builtin throw)
    , ("next", builtin $ const next)
    , ("exit", builtin exit_)
    ]


-- functions
sort :: Function
sort (List v:_) = do
    v' <- liftIO $ do
        vm <- V.thaw v
        VI.sort vm
        V.unsafeFreeze vm
    return $ List v'
sort (d@(Dict _):args) = sort (list d:args)
sort (Unit:args) = sort (list Unit:args)
sort (obj:_) = exception $ String $ "sort: not a List: " ++. showText obj


-- operators
__add__ :: Function
__add__ = numop (+)

__app__ :: Function
__app__ (func@(Function _ _):args) = funcCall func args
__app__ (obj:_) = exception $ String $ "not a function " ++. showText obj

__div__ :: Function
__div__ = numop (/)

__mul__ :: Function
__mul__ = numop (*)

__num__ :: Function
__num__ (obj:_) = return $ num obj

__neg__ :: Function
__neg__ (Number n:_) = return $ Number $ -n
__neg__ (obj:_) = exception $ String $ "not a number: " ++. showText obj

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

__not__ :: Function
__not__ (obj:_) =
    let Bool b = bool obj
    in return $ Bool (not b)

__and__ :: Function
__and__ (left:right:_) | isTrue left && isTrue right = return right
__and__ (left:_) = return left

__or__ :: Function
__or__ (left:right:_) | isTrue left = return left
                      | otherwise = return right


__get__ :: Function
__get__ (Unit:_) = return Unit
__get__ (Dict h:String name:names@(String _:_)) = __get__ $ H.lookupDefault Unit name h : names
__get__ (Dict h:String name:_) = return $ H.lookupDefault Unit name h
__get__ (obj:_) = exception $ String $ "__get__: not a Dict: " ++. showText obj

__if__ :: Function
__if__ (b:block:_) =
    case bool b of
        Bool True  -> funcCall block []
        Bool False -> return Unit

__wref__ :: Function
__wref__ (Ref ref:obj:_) = writeRef ref obj >> return obj
__wref__ (obj:_) = exception $ String $ "__wref__: not a Ref: " ++. showText obj

__match__ :: Function
__match__ (String str:String pat:_) =
    case regex' [] pat of
        Left e  -> exception $ String $ "__match__: parse error: " ++. showText e
        Right r -> return $ Bool $ isJust $ find r str
__match__ (Unit:args) = __match__ (str Unit : args)
__match__ (String _:obj:_) = exception $ String $ "__match__: not a String: " ++. showText obj
__match__ (obj:_) = exception $ String $ "__match__: not a String: " ++. showText obj

__nmatch__ :: Function
__nmatch__ args = do
    Bool b <- __match__ args
    return $ Bool $ not b


-- controls
-- TODO: else
if_ :: Function
if_ (b:_) = return . Function Builtin $ \(block:_) ->
    case bool b of
        Bool True  -> funcCall block []
        Bool False -> return Unit

for :: Function
for (List v:_) = return . Function Builtin $ \(block:_) -> V.foldM (\_ obj -> funcCall block [obj]) Unit v
for (d@(Dict _):args) = for (list d:args)
for (Unit:args) = for (list Unit:args)
for _ = exception $ String "for: not implemented"

while :: Function
while (cond:_) = return . Function Builtin $ \(block:_) -> go block Unit
  where
    go block ret = do
        Bool b <- bool <$> funcCall cond []
        if b
          then funcCall block [] >>= go block
          else return ret

return_ :: Function
return_ (ret:_) = throwError $ Return ret

print :: Function
print args = do
    lift $ yield args
    return $ Bool True

getobj :: Function
getobj _ = do
    mo <- lift await
    case mo of
        Just o  -> return o
        Nothing -> return Unit

catch_ :: Function
catch_ (body:_) = funcCall body [] `catchError` catch'
  where
    catch' (Exception obj) = return obj
    catch' e = throwError e

throw :: Function
throw (obj:_) = exception obj

exit_ :: Function
exit_ (Number (I i):_) = exit $ fromInteger i
exit_ (Number (D d):_) = exit $ error "exit with Double: not implemented" d
exit_ _ = exit 0

numop :: (Number -> Number -> Number) -> Function
numop op (Number l:Number r:_) = return $ Number $ l `op` r
numop op (n@(Number _):Unit:_) = numop op [n, num Unit]
numop op (Unit:n@(Number _):_) = numop op [num Unit, n]
numop _ (Number _:obj) = exception $ String $ "not a number: " ++. showText obj
numop _ (obj:_) = exception $ String $ "not a number: " ++. showText obj

mkOp :: (Object -> Object -> Object) -> Function
mkOp op (left:right:_) = return $ op left right

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
