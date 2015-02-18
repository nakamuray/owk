{- http://www.haskell.org/haskellwiki/Poor_man%27s_here_document#Quasiquoting -}
module Owk.Test.Util where

import Data.Conduit
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Data.Conduit.List as CL

import Owk.Builtin (builtins)
import Owk.Type as Type

import qualified Owk.Namespace as Namespace

s = QuasiQuoter { quoteExp = stringE }


testOwk owk = do
    n <- Namespace.fromList builtins
    runOwk owk n

testOwk_ owk = testOwk owk >> return ()
