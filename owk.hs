{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Conduit

import Control.Applicative ((<$>), (<*>))
import System.Environment (getArgs)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdin, stdout)
import Text.Show.Pretty (ppShow)

import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import qualified Data.Text.IO as TI

import Data.Conduit.Owk
import Data.Conduit.Owk.Type
import qualified Data.Conduit.Owk.ApacheLog as ApacheLog
import qualified Data.Conduit.Owk.Line as Line
import qualified Data.Conduit.Owk.LTSV as LTSV
import qualified Data.Conduit.Owk.JSON as JSON
import Owk.Parser


main :: IO ()
main = do
    config <- parseArgs <$> getArgs
    case config of
        Config { showHelp = True } -> putStrLn usage
        _ -> run config

run :: Config -> IO ()
run config@(Config _ [] _ _) = run config { owkScripts = [defaultScript] }
run (Config _ scripts i o) = do
    owks <- mapM scriptToOwk scripts
    hSetBuffering stdout LineBuffering
    let source = CB.sourceHandle stdin $= i
        sink = o =$ CB.sinkHandle stdout

        owk' = foldl1 (=$=) owks
    source $= owk' $$ sink
  where
    scriptToOwk (Eval (ScriptText script fname)) = owk fname <$> script
    scriptToOwk (Map (ScriptText script fname)) = owkMap fname <$> script
    scriptToOwk (Fold (ScriptText script fname)
                      (ScriptText initscript fname')) = owkFold fname <$> script <*> initscript
    scriptToOwk (Dump (ScriptText script fname)) = do
        ret <- parseOwk fname <$> script
        case ret of
            Right prog -> pprint prog
            Left  e    -> putStrLn e
        return $ return ()


inputs :: [(String, OwkInput)]
inputs =
    [ ("line", Line.toObject)
    , ("ltsv", LTSV.toObject)
    , ("json", JSON.toObject)
    , ("apachelog", ApacheLog.toObject)
    ]

outputs :: [(String, OwkOutput)]
outputs =
    [ ("line", Line.fromObjects)
    , ("ltsv", LTSV.fromObjects)
    , ("json", JSON.fromObjects)
    , ("jsonpp", JSON.fromObjectsPretty)
    ]

usage :: String
usage = unlines
    [ "usage: owk [OPTIONS] <owk script>"
    , "   or: owk [OPTIONS] [-f <script file>]"
    , ""
    , "OPTIONS: -h        print this help"
    , "         -d        dump AST and exit"
    , "         -e        eval mode"
    , "         -m        map mode (default)"
    , "         -r        reduce (fold) mode"
    , "         -mf       map mode (read from script file)"
    , "         -rf       reduce mode (read from script file)"
    , "         -i TYPE   set input TYPE"
    , "         -o TYPE   set output TYPE"
    , "         -io TYPE  set both input/output"
    , ""
    , "TYPE: line (default)"
    , "      ltsv"
    , "      json"
    , "      jsonpp (output only)"
    , "      apachelog (input only)"
    ]

pprint :: Show a => a -> IO ()
pprint = putStrLn . ppShow

data ScriptText = ScriptText (IO T.Text) FilePath
data OwkScript = Eval ScriptText
          | Map ScriptText
          | Fold ScriptText ScriptText
          | Dump ScriptText

defaultScript :: OwkScript
defaultScript = Eval $ ScriptText TI.getContents "<stdin>"

data Config = Config
  { showHelp :: Bool
  , owkScripts :: [OwkScript]
  , owkInput :: OwkInput
  , owkOutput :: OwkOutput
  }

defaultConfig :: Config
defaultConfig = Config False [] Line.toObject Line.fromObjects

parseArgs :: [String] -> Config
parseArgs args = parseArgs' defaultConfig args

parseArgs' :: Config -> [String] -> Config
parseArgs' config [] = config
parseArgs' config ("-h":args) = parseArgs' config { showHelp = True } args
parseArgs' config ("-d":script:args) = parseArgs' config { owkScripts = owkScripts config ++ [Dump $ stext script] } args
parseArgs' config ("-e":script:args) = parseArgs' config { owkScripts = owkScripts config ++ [Eval $ stext script] } args
parseArgs' config ("-m":script:args) = parseArgs' config { owkScripts = owkScripts config ++ [Map $ stext script] } args
parseArgs' config ("-r":script:[]) = parseArgs' config { owkScripts = owkScripts config ++ [Fold (stext script) (stext "")] } []
parseArgs' config ("-r":script:args@(('-':_):_)) = parseArgs' config { owkScripts = owkScripts config ++ [Fold (stext script) (stext "")] } args
parseArgs' config ("-r":script:initscript:args) = parseArgs' config { owkScripts = owkScripts config ++ [Fold (stext script) (stext initscript)] } args
parseArgs' config ("-f":fname:args) = parseArgs' config { owkScripts = owkScripts config ++ [Eval $ ScriptText (TI.readFile fname) fname] } args
parseArgs' config ("-mf":fname:args) = parseArgs' config { owkScripts = owkScripts config ++ [Map $ ScriptText (TI.readFile fname) fname] } args
parseArgs' config ("-rf":fname:args) = parseArgs' config { owkScripts = owkScripts config ++ [Fold (ScriptText (TI.readFile fname) fname) (stext "")] } args
parseArgs' config ("-i":pname:args) =
    case lookup pname inputs of
        Just input -> parseArgs' config { owkInput = input } args
        Nothing   -> error $ "unknown TYPE name: " ++ pname
parseArgs' config ("-o":pname:args) =
    case lookup pname outputs of
        Just output -> parseArgs' config { owkOutput = output } args
        Nothing   -> error $ "unknown TYPE name: " ++ pname
parseArgs' config ("-io":pname:args) = parseArgs' config $ "-i":pname:"-o":pname:args
parseArgs' config (script:args) = parseArgs' config { owkScripts = owkScripts config ++ [Map $ stext script] } args

stext :: String -> ScriptText
stext s = ScriptText (return $ T.pack s) "<string>"
