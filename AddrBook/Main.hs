module Main (main) where

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Console.GetOpt
import System.Directory
import System.Environment

import AddrBook.CreateDB
import AddrBook.Err

data Flag = Path String | Create

flags :: [OptDescr Flag]
flags = 
    [ Option ['p'] [] (ReqArg Path "PATH") []
    , Option ['c'] [] (NoArg Create) [] ]

main :: IO ()
main = do
    argv <- getArgs
    let opts = getOpt RequireOrder flags argv
    case opts of
        (o, _, [])  -> do
            let path = isPath o
                make = isCreate o
            case make of
                True  -> connectDB path >>= createDB >>= mainLoop
                False -> connectDB path >>= mainLoop
        (_, _, e)   -> err 1 usage
        (_, _, [])  -> connectDB Nothing >>= mainLoop

  where isPath []            = Nothing
        isPath ((Path x):fs) = Just x
        isPath (_:fs)        = isPath fs

        isCreate []          = False
        isCreate (Create:fs) = True
        isCreate (_:fs)      = isCreate fs

connectDB :: Maybe FilePath -> IO Connection
connectDB Nothing = do
    home <- getHomeDirectory
    con  <- connectSqlite3 $ home ++ "/.addrbook.db"
    runRaw con "pragma foreign_keys = on;"
    return con
connectDB (Just home) = do
    -- if it's a directory, append name to end
    isDir <- doesDirectoryExist home
    let path = if isDir
               then path ++ "/.addrbook.db"
               else home
    con <- connectSqlite3 $ path
    runRaw con "pragma foreign_keys = on;"
    return con

usage :: String
usage = "usage: addrbook [-c | -p path]"

mainLoop = undefined
