module Main (main) where

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory
import System.Environment
import System.IO
import Text.Parsec

import AddrBook.AddrBook
import AddrBook.CreateDB
import AddrBook.Usage

main :: IO ()
main = do
    argv <- getArgs
    case argv of
        ("-d":path:[]) -> connectDB (Just path) >>= mainLoop
        ("-c":[])      -> connectDB Nothing >>= createDB
        []             -> connectDB Nothing >>= mainLoop
        -- unrecognized options
        _              -> usage Nothing True

mainLoop :: IConnection c => c -> IO ()
mainLoop con = do
    putStr "? "
    hFlush stdout
    input <- getLine 
    res   <- 
        runParserT (addrBookLoop con) [] "addrbook" input
    case res of
        (Left parseError) -> 
            hPutStr stderr (show parseError) >>
            mainLoop con
        (Right success)   -> mainLoop con

connectDB :: Maybe FilePath -> IO Connection
connectDB Nothing = do
    home <- getHomeDirectory
    con  <- connectSqlite3 $ home ++ "/.addrbook.db"
    runRaw con "pragma foreign_keys = on;"
    return con
connectDB (Just home) = do
    -- make sure they didn't give us a directory
    isDir <- doesDirectoryExist home
    case isDir of
        True  -> do 
            con <- connectSqlite3 $ home ++ "/.addrbook.db"
            runRaw con "pragma foreign_keys = on;"
            return con
        False -> do 
            con <- connectSqlite3 home
            runRaw con "pragma foreign_keys = on;"
            return con
