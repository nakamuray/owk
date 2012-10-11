import Test.Framework (defaultMain)

import Owk.Test.Type as Type
import Owk.Test.Builtin as Builtin

main :: IO ()
main = defaultMain
    [ Type.tests
    , Builtin.tests
    ]
