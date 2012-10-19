{- http://www.haskell.org/haskellwiki/Poor_man%27s_here_document#Quasiquoting -}
module Owk.Test.Util where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

s = QuasiQuoter { quoteExp = stringE }
