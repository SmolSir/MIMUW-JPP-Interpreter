import System.IO
import System.Environment
import System.Exit

import Control.Monad.Reader

import Grammar.Par

import qualified Interpreter

interpreter :: String -> IO ( )
interpreter plainText = do
    case pProgram $ myLexer plainText of
        Left  errorMsg -> hPutStrLn stderr errorMsg
        Right program  -> Interpreter.runProgram program
            -- TODO: add the type-checker here

main :: IO ( )
main = do
    programArgs <- System.Environment.getArgs
    case programArgs of
        [filePath] -> readFile filePath >>= interpreter
        []         -> putStrLn "TODO" >> getContents >>= interpreter
        _          -> die "TODO"
