{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Conduit

import Control.Applicative ((<$>))
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Control.Monad.Error (catchError, throwError)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import System.Exit (ExitCode(..), exitFailure, exitSuccess, exitWith)
import Text.Show.Pretty (ppShow)

import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.HashMap.Strict as H

import Owk.AST as AST
import Owk.Builtin (builtins)
import Owk.Interpreter
import Owk.IO.Type
import qualified Owk.IO.Line as Line
import qualified Owk.IO.JSON as JSON
import qualified Owk.Namespace as Namespace
import Owk.Parser
import Owk.Type as Type


main :: IO ()
main = do
    config <- parseArgs <$> getArgs
    case config of
        Config { showHelp = True } -> putStrLn usage
        Config { dumpAST = True
               , fileName = fname
               , owkScript = script } -> pprint . parseOwk fname =<< script
        config -> run config

run :: Config -> IO ()
run (Config _ _ em fname script i o) = do
    script' <- script
    n <- Namespace.fromList builtins
    case parseOwk fname script' of
        Left e     -> hPutStrLn stderr e >> exitFailure
        Right prog -> do
            let prog' = if em then AST.Program $ [AST.Define "main" $ AST.Function ["$"] $ unProg prog]
                              else prog
                source = CB.sourceHandle stdin $= i
                sink = o =$ CB.sinkHandle stdout
            -- update global namespace
            (next, _) <- source $$+ runOwk (interpret_ prog' `catchError` catchExit) n =$ sink
            mmain <- Namespace.lookupIO "main" $ Type.Global n
            case mmain of
                Nothing    -> error "no `main` found"
                Just owkMain -> do
                    let owkMain' = awaitForever $ \obj -> do
                        runOwk (funcCall owkMain [obj] `catchError` ignoreNext `catchError` catchExit >> return ()) n
                        return ()
                    next $$++ owkMain' =$ sink
                    mend <- Namespace.lookupIO "end" $ Type.Global n
                    case mend of
                        Just end -> do
                            CL.sourceNull $= runOwk (funcCall end [] `catchError` ignoreNext `catchError` catchExit >> return ()) n $$ sink
                            return ()
                        Nothing  -> return ()
  where
    unProg (Program es) = es

    ignoreNext Next = return Type.Unit
    ignoreNext e    = throwError e

    catchExit (Exit 0) = liftIO exitSuccess
    catchExit (Exit i) = liftIO $ exitWith $ ExitFailure i
    catchExit e        = throwError e

iopipes :: [(String, IOPipe)]
iopipes =
  [ ("line", Line.iopipe)
  , ("json", JSON.iopipe)
  ]

usage :: String
usage = unlines
    [ "usage: owk [OPTIONS] <owk script>"
    , "   or: owk [OPTIONS] [-f <script name>]"
    , ""
    , "OPTIONS: -h        print this help"
    , "         -d        dump AST and exit"
    , "         -m        explicit `main = $ -> { ... }` function definition"
    , "         -i TYPE   use TYPE input decoder"
    , "         -o TYPE   use TYPE output encoder"
    , "         -io TYPE  set both decoder/encoder"
    ]

pprint :: Show a => a -> IO ()
pprint = putStrLn . ppShow

data Config = Config
  { showHelp :: Bool
  , dumpAST :: Bool
  , explicitMain :: Bool
  , fileName :: FilePath
  , owkScript :: IO T.Text
  , ioInput :: Input
  , ioOutput :: Output
  }

defaultConfig :: Config
defaultConfig = Config False False False "<stdin>" TI.getContents (input Line.iopipe)  (output Line.iopipe)

parseArgs :: [String] -> Config
parseArgs args = parseArgs' defaultConfig args

parseArgs' :: Config -> [String] -> Config
parseArgs' config [] = config
parseArgs' config ("-h":args) = parseArgs' config { showHelp = True } args
parseArgs' config ("-d":args) = parseArgs' config { dumpAST = True } args
parseArgs' config ("-m":args) = parseArgs' config { explicitMain = True } args
parseArgs' config ("-f":fname:args) = parseArgs' config { fileName = fname, owkScript = TI.readFile fname } args
parseArgs' config ("-i":pname:args) =
    case lookup pname iopipes of
        Just pipe -> parseArgs' config { ioInput = input pipe} args
        Nothing   -> error $ "unknown TYPE name: " ++ pname
parseArgs' config ("-o":pname:args) =
    case lookup pname iopipes of
        Just pipe -> parseArgs' config { ioOutput = output pipe} args
        Nothing   -> error $ "unknown TYPE name: " ++ pname
parseArgs' config ("-io":pname:args) = parseArgs' config $ "-i":pname:"-o":pname:args
parseArgs' config (script:args) = parseArgs' config { fileName = "<string>", owkScript = return $ T.pack script } args
