{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Control.Monad.Error (catchError, throwError)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import System.Exit (ExitCode(..), exitFailure, exitSuccess, exitWith)
import Text.Show.Pretty (ppShow)

import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.HashMap.Strict as H

import Owk.AST as AST
import Owk.Builtin (builtins, makePrint)
import Owk.Interpreter
import Owk.IO.Plugin
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
run (Config _ _ em fname script r w) = do
    script' <- script
    n <- Namespace.fromList builtins
    Namespace.insertIO "print" (makePrint $ w stdout) n
    case parseOwk fname script' of
        Left e     -> hPutStrLn stderr e >> exitFailure
        Right prog -> do
            let prog' = if em then AST.Program $ [AST.Define "main" $ AST.Function ["$"] $ unProg prog]
                              else prog
            -- update global namespace
            runOwk (interpret prog' `catchError` catchExit) n >> return ()
            mmain <- Namespace.lookupIO "main" $ Type.Global n
            case mmain of
                Nothing    -> error "no `main` found"
                Just owkMain -> do
                    r stdin $ \inputs -> do
                        flip runOwk n $ funcCall owkMain inputs `catchError` ignoreNext `catchError` catchExit
                        return ()
                    mend <- Namespace.lookupIO "end" $ Type.Global n
                    case mend of
                        Just end -> do
                            flip runOwk n $ funcCall end []
                            return ()
                        Nothing  -> return ()
  where
    unProg (Program es) = es

    ignoreNext Next = return Type.Unit
    ignoreNext e    = throwError e

    catchExit (Exit 0) = liftIO exitSuccess
    catchExit (Exit i) = liftIO $ exitWith $ ExitFailure i
    catchExit e        = throwError e

ioplugins :: [(String, IOPlugin)]
ioplugins =
  [ ("line", Line.ioplugin)
  , ("json", JSON.ioplugin)
  ]

usage :: String
usage = unlines
    [ "usage: owk [OPTIONS] <owk script>"
    , "   or: owk [OPTIONS] [-f <script name>]"
    , ""
    , "OPTIONS: -h          print this help"
    , "         -d          dump AST and exit"
    , "         -m          explicit `main = $ -> { ... }` function definition"
    , "         -i PLUGIN   use PLUGIN as a input decoder"
    , "         -o PLUGIN   use PLUGIN as a output encoder"
    , "         -io PLUGIN  set both decoder/encoder"
    ]

pprint :: Show a => a -> IO ()
pprint = putStrLn . ppShow

data Config = Config
  { showHelp :: Bool
  , dumpAST :: Bool
  , explicitMain :: Bool
  , fileName :: FilePath
  , owkScript :: IO T.Text
  , ioReader :: IOReader
  , ioWriter :: IOWriter
  }

defaultConfig :: Config
defaultConfig = Config False False False "<stdin>" TI.getContents (reader Line.ioplugin)  (writer Line.ioplugin)

parseArgs :: [String] -> Config
parseArgs args = parseArgs' defaultConfig args

parseArgs' :: Config -> [String] -> Config
parseArgs' config [] = config
parseArgs' config ("-h":args) = parseArgs' config { showHelp = True } args
parseArgs' config ("-d":args) = parseArgs' config { dumpAST = True } args
parseArgs' config ("-m":args) = parseArgs' config { explicitMain = True } args
parseArgs' config ("-f":fname:args) = parseArgs' config { fileName = fname, owkScript = TI.readFile fname } args
parseArgs' config ("-i":pname:args) =
    case lookup pname ioplugins of
        Just plugin -> parseArgs' config { ioReader = reader plugin} args
        Nothing     -> error $ "unknown plugin name: " ++ pname
parseArgs' config ("-o":pname:args) =
    case lookup pname ioplugins of
        Just plugin -> parseArgs' config { ioWriter = writer plugin} args
        Nothing     -> error $ "unknown plugin name: " ++ pname
parseArgs' config ("-io":pname:args) = parseArgs' config $ "-i":pname:"-o":pname:args
parseArgs' config (script:args) = parseArgs' config { fileName = "<string>", owkScript = return $ T.pack script } args
