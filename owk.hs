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
import qualified Data.Conduit.Owk.Line as Line
import qualified Data.Conduit.Owk.LTSV as LTSV
import qualified Data.Conduit.Owk.JSON as JSON
import Owk.Parser


main :: IO ()
main = do
    config <- parseArgs <$> getArgs
    case config of
        Config { showHelp = True } -> putStrLn usage
        Config { dumpAST = True
               , fileName = fname
               , owkScript = script } -> pprint . parseOwk fname =<< script
        _ -> run config

run :: Config -> IO ()
run (Config _ _ em fname script i o) = do
    script' <- script
    hSetBuffering stdout LineBuffering
    let source = CB.sourceHandle stdin $= i
        sink = o =$ CB.sinkHandle stdout

        owk' = if em then owkMain fname script'
                     else owk fname script'
    source $= owk' $$ sink


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
    , "   or: owk [OPTIONS] [-f <script name>]"
    , ""
    , "OPTIONS: -h        print this help"
    , "         -d        dump AST and exit"
    , "         -m        explicit `main = $ -> { ... }` function definition"
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

data Config = Config
  { showHelp :: Bool
  , dumpAST :: Bool
  , explicitMain :: Bool
  , fileName :: FilePath
  , owkScript :: IO T.Text
  , owkInput :: OwkInput
  , owkOutput :: OwkOutput
  }

defaultConfig :: Config
defaultConfig = Config False False False "<stdin>" TI.getContents Line.toObject Line.fromObjects

parseArgs :: [String] -> Config
parseArgs args = parseArgs' defaultConfig args

parseArgs' :: Config -> [String] -> Config
parseArgs' config [] = config
parseArgs' config ("-h":args) = parseArgs' config { showHelp = True } args
parseArgs' config ("-d":args) = parseArgs' config { dumpAST = True } args
parseArgs' config ("-m":args) = parseArgs' config { explicitMain = True } args
parseArgs' config ("-f":fname:args) = parseArgs' config { fileName = fname, owkScript = TI.readFile fname } args
parseArgs' config ("-i":pname:args) =
    case lookup pname inputs of
        Just input -> parseArgs' config { owkInput = input } args
        Nothing   -> error $ "unknown TYPE name: " ++ pname
parseArgs' config ("-o":pname:args) =
    case lookup pname outputs of
        Just output -> parseArgs' config { owkOutput = output } args
        Nothing   -> error $ "unknown TYPE name: " ++ pname
parseArgs' config ("-io":pname:args) = parseArgs' config $ "-i":pname:"-o":pname:args
parseArgs' config (script:args) = parseArgs' config { fileName = "<string>", owkScript = return $ T.pack script } args
