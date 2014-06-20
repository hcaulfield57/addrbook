module AddrBook.PrintDB
    ( printUserIndex
    , printPhones
    , printEmails
    , printAddresses
    , printMisc
    ) where

import Database.HDBC

import AddrBook.Types

printUserIndex :: Int -> [Dot] -> IO ()
printUserIndex _ [] = return ()
printUserIndex i dot@(u:us) = do
    putStr $ show i ++ " "
    putStr $ firstName u ++ ", "
    putStrLn $ lastName u
    printUserIndex (i+1) us

printPhones :: Int -> [Dot] -> IO ()
printPhones _ [] = return ()
printPhones i (p:ps) = do
    putStr $ show i ++ " "
    putStr $ phoneNumber p
    if not $ null (phoneType p)
        then do
            putStrLn $ " - " ++ phoneType p
            printPhones (i+1) ps
        else do
            putStrLn ""
            printPhones (i+1) ps

printEmails :: Int -> [Dot] -> IO ()
printEmails _ [] = return ()
printEmails i (e:es) = do
    putStr $ show i ++ " "
    putStrLn $ emailAddr e
    printEmails (i+1) es

printAddresses :: Int -> [Dot] -> IO ()
printAddresses _ [] = return ()
printAddresses i (a:as) = do
    putStr $ show i ++ " "
    putStrLn $ addressAddr a
    printAddresses (i+1) as

printMisc :: Int -> [Dot] -> IO ()
printMisc _ [] = return ()
printMisc i (m:ms) = do
    putStr $ show i ++ " "
    putStrLn $ miscInfo m
    printMisc (i+1) ms
