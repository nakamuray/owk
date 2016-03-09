{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Conduit

import Control.Applicative ((<$>))
import System.Environment (getArgs)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdin, stdout)
import Text.Show.Pretty (ppShow)

import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import qualified Data.Text.IO as TI

import Data.Conduit.Owk
import Data.Conduit.Owk.Type
import qualified Data.Conduit.Owk.ApacheLog as ApacheLog
import qualified Data.Conduit.Owk.Word as Word
import qualified Data.Conduit.Owk.Line as Line
import qualified Data.Conduit.Owk.CSV as CSV
import qualified Data.Conduit.Owk.LTSV as LTSV
import qualified Data.Conduit.Owk.JSON as JSON
import Owk.Parser


main :: IO ()
main = do
    config <- parseArgs <$> getArgs
    run config

run :: Config -> IO ()
run (Config mode scriptText i o) = do
    owk' <- scriptToOwk mode scriptText
    hSetBuffering stdout LineBuffering
    let source = CB.sourceHandle stdin $= i
        sink = o =$ CB.sinkHandle stdout
    owk' source $$ sink
  where
    scriptToOwk Eval (ScriptText script fname) = owkEval fname <$> script
    scriptToOwk Map (ScriptText script fname) = owk fname <$> script
    scriptToOwk Dump (ScriptText script fname) = do
        ret <- parseOwk fname <$> script
        case ret of
            Right prog -> pprint prog
            Left  e    -> putStrLn e
        return $ const $ return ()
    scriptToOwk Help _ = do
        putStrLn usage
        return $ const $ return ()


inputs :: [(String, OwkInput)]
inputs =
    [ ("line", Line.toObject)
    , ("word", Word.toObject)
    , ("csv", CSV.toObject)
    , ("ltsv", LTSV.toObject)
    , ("json", JSON.toObject)
    , ("jsonpp", JSON.toObject)
    , ("apachelog", ApacheLog.toObject)
    ]

outputs :: [(String, OwkOutput)]
outputs =
    [ ("line", Line.fromObjects)
    , ("word", Word.fromObjects)
    , ("csv", CSV.fromObjects)
    , ("ltsv", LTSV.fromObjects)
    , ("json", JSON.fromObjects)
    , ("jsonpp", JSON.fromObjectsPretty)
    ]

usage :: String
usage = unlines
    [ "usage: owk [OPTIONS] <owk expressions>"
    , "   or: owk [OPTIONS] [-f <script file>]"
    , ""
    , "OPTIONS: -h                     print this help"
    , "         -d <owk expressions>   dump AST and exit"
    , "         -e <owk expressions>   eval mode"
    , "         -m <owk expressions>   map mode (default)"
    , "         -f <script file>       eval mode (read from script file)"
    , "         -i TYPE                set input TYPE"
    , "         -o TYPE                set output TYPE"
    , "         -io TYPE               set both input/output"
    , ""
    , "TYPE: line"
    , "      word"
    , "      csv"
    , "      ltsv"
    , "      json"
    , "      jsonpp (output only) [default]"
    , "      apachelog (input only)"
    ]

pprint :: Show a => a -> IO ()
pprint = putStrLn . ppShow

data ScriptText = ScriptText (IO T.Text) FilePath
data Mode = Help | Eval | Map | Dump

defaultScript :: ScriptText
defaultScript = ScriptText TI.getContents "<stdin>"

data Config = Config
  { owkMode :: Mode
  , owkScript :: ScriptText
  , owkInput :: OwkInput
  , owkOutput :: OwkOutput
  }

defaultConfig :: Config
defaultConfig = Config Map defaultScript JSON.toObject JSON.fromObjectsPretty

parseArgs :: [String] -> Config
parseArgs args = parseArgs' defaultConfig args

parseArgs' :: Config -> [String] -> Config
parseArgs' config [] = config
parseArgs' config ("-h":args) = parseArgs' config { owkMode = Help } args
parseArgs' config ("-d":args) = parseArgs' config { owkMode = Dump } args
parseArgs' config ("-e":args) = parseArgs' config { owkMode = Eval } args
parseArgs' config ("-m":args) = parseArgs' config { owkMode = Map } args
parseArgs' config ("-f":fname:args) = parseArgs' config { owkMode = Eval, owkScript = ScriptText (TI.readFile fname) fname } args
parseArgs' config ("-i":pname:args) =
    case lookup pname inputs of
        Just input -> parseArgs' config { owkInput = input } args
        Nothing   -> error $ "unknown TYPE name: " ++ pname
parseArgs' config ("-o":pname:args) =
    case lookup pname outputs of
        Just output -> parseArgs' config { owkOutput = output } args
        Nothing   -> error $ "unknown TYPE name: " ++ pname
parseArgs' config ("-io":pname:args) = parseArgs' config $ "-i":pname:"-o":pname:args
parseArgs' config (script:args) = parseArgs' config { owkScript = stext script } args

stext :: String -> ScriptText
stext s = ScriptText (return $ T.pack s) "<string>"
