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

main :: IO ( )
main = do
    programArgs <- System.Environment.getArgs
    case programArgs of
        [filePath] -> readFile filePath >>= interpreter
        []         -> putStrLn "no command line arguments provided -- reading from stdin..."
                        >> getContents >>= interpreter
        _          -> die "invalid file path. Provide a valid path or nothing to read from stdin"
