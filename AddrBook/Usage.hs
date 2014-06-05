module AddrBook.Usage (usage) where

import System.Exit
import System.IO

usage 
    :: Maybe String   -- Optional error message
    -> Bool           -- Print usage information?
    -> IO ()
usage Nothing True =
    hPutStr stderr "usage: addrbook [-d filepath | -c]\n" >>
    exitWith (ExitFailure 1)
usage (Just msg) False =
    hPutStrLn stderr msg >>
    exitWith (ExitFailure 1)
usage (Just msg) True =   
    hPutStrLn stderr msg                                  >>
    hPutStr stderr "usage: addrbook [-d filepath | -c]\n" >>
    exitWith (ExitFailure 1)
usage Nothing False = error "this should never be called"
