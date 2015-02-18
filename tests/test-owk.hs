import Test.Framework (defaultMain)

import Owk.Test.Type as Type
import Owk.Test.Parser as Parser
import Owk.Test.Interpreter as Interpreter
import Owk.Test.Builtin as Builtin
--import Owk.Test.Main as Main

main :: IO ()
main = defaultMain
    [ Type.tests
    , Parser.tests
    , Interpreter.tests
    , Builtin.tests
    -- FIXME:
    --, Main.tests
    ]
