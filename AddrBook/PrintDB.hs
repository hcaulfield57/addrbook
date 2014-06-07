module AddrBook.PrintDB
    ( printUserIndex
    , printPhones
    , printEmails
    ) where

import Control.Monad (when)
import Database.HDBC

import AddrBook.Types

printUserIndex :: [Dot] -> IO ()
printUserIndex [] = return ()
printUserIndex (u:us) = do
    putStr $ (show $ personId u) ++ " "
    putStr $ firstName u ++ ", "
    putStrLn $ lastName u
    printUserIndex us

printPhones :: [Dot] -> IO ()
printPhones [] = return ()
printPhones (p:ps) = do
    putStr $ (show $ phoneId p) ++ " "
    putStr $ phoneNumber p
    if not $ null (phoneType p)
        then do
            putStrLn $ " - " ++ phoneType p
            printPhones ps
        else do
            putStrLn ""
            printPhones ps

printEmails :: [Dot] -> IO ()
printEmails [] = return ()
printEmails (e:es) = do
    putStr $ (show $ emailId e) ++ " "
    putStrLn $ emailAddr e
    printEmails es
