module Main where

import System.Cmd
import System.Environment
import System.FilePath.Posix
import System.Exit
import System.IO
import Text.Printf
import AbsLatte
import LexLatte
import ParLatte
import ErrM
import Frontend
import Backend


exitWithError :: String -> IO ()
exitWithError errMessage = do
    hPutStrLn stderr "ERROR"
    hPutStrLn stderr errMessage
    exitFailure

compileProgram :: Program -> IO ()
compileProgram program = do
    (result, finalStore) <- runEval emptyStore (emitProgram program)
    case result of
        Left message -> 
        Right () -> do
            let text = prepareProgText (instructions finalStore)
            let printFun = \handle -> sequence_ $ map (hPutStrLn handle) text
            withFile outputFile WriteMode printFun
            let bcFile = replaceExtension outputFile ".bc"
            exitCode <- system $ printf "llvm-as -o %s %s" bcFile outputFile
            return ()

parseAndCompile :: String -> FilePath -> IO ()
parseAndCompile progText outputFile = do
    let parse = pProgram (myLexer progText)
    case parse of
        Ok program -> compileProgram program
        Bad message -> exitWithError message

main = do
    args <- getArgs
    if length args /= 1 then
        exitWithError "Wrong number of arguments"
    else do
        let fileName = args !! 0
        let outputFile = replaceExtension fileName ".ll"
        progText <- readFile $ fileName
        parseAndCompile progText outputFile
