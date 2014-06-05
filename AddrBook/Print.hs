module AddrBook.Print (printAll, printInfo) where

import Database.HDBC

import AddrBook.SelectDB

printAll :: [[SqlValue]] -> IO ()
printAll res = mapM_ printAll' res
  where printAll' (r:f:l:[]) = do
            putStr $ fromSql r ++ " "
            putStr $ fromSql l
            putStr ","
            putStrLn $ " " ++ fromSql f
        printAll' (r:l:[]) = do
            putStr $ fromSql r ++ " "
            putStrLn $ fromSql l
        printAll' [] = return ()

printInfo :: Int -> IO ()
printInfo num = do
    res <- selectInfo con num
    -- TODO
    return ()
