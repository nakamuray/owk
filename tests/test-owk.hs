import Test.Framework (defaultMain)

import Owk.Test.Type as Type
import Owk.Test.Parser as Parser
import Owk.Test.Interpreter as Interpreter
import Owk.Test.Builtin as Builtin

main :: IO ()
main = defaultMain
    [ Type.tests
    , Parser.tests
    , Interpreter.tests
    , Builtin.tests
    ]
