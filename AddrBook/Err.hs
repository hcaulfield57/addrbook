module AddrBook.Err (err) where

import System.Environment
import System.Exit
import System.IO

err :: Int -> String -> IO ()
err code errMsg = do
    arg0 <- getProgName
    hPutStrLn stderr $ arg0 ++ ": " ++ errMsg
    exitWith (ExitFailure code)
