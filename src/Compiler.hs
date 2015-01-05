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
import qualified Frontend(runEval, checkProgram, emptyEnv)
import qualified Backend(runEval, emitProgram, emptyEnv, emptyStore, compiled)


exitWithError :: String -> IO ()
exitWithError errMessage = do
    hPutStrLn stderr "ERROR"
    hPutStrLn stderr errMessage
    exitFailure

compileProgram :: Program -> FilePath -> IO ()
compileProgram program outputFile = do
    result  <- Frontend.runEval Frontend.emptyEnv (Frontend.checkProgram program)
    case result of
        Left message -> exitWithError message
        Right optimizedProgram -> do
            hPutStrLn stderr "OK"
            (result', finalStore) <- Backend.runEval Backend.emptyEnv Backend.emptyStore (Backend.emitProgram optimizedProgram)
            let printFun = \handle -> sequence_ $ map (hPutStrLn handle) (Backend.compiled finalStore)
            withFile outputFile WriteMode printFun
            let bcFile = replaceExtension outputFile ".bc"
            _ <- system $ printf "llvm-as -o %s %s" bcFile outputFile
            _ <- system $ printf "llvm-link -o %s %s lib/runtime.bc" bcFile bcFile
            return ()

parseAndCompile :: String -> FilePath -> IO ()
parseAndCompile progText outputFile = do
    let parse = pProgram (myLexer progText)
    case parse of
        Ok program -> compileProgram program outputFile
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
