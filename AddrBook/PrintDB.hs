module AddrBook.PrintDB
    ( printUserIndex
    , printPhones
    ) where

import Database.HDBC

import AddrBook.Types

printUserIndex :: [User] -> IO ()
printUserIndex [] = return ()
printUserIndex (u:us) = do
    putStr $ (show $ personId u) ++ " "
    putStr $ firstName u ++ ", "
    putStrLn $ lastName u
    printUserIndex us

printPhones :: [Phone] -> IO ()
printPhones [] = return ()
printPhones (p:ps) = do
    putStr $ (show $ phoneId p) ++ " "
    putStr $ phoneNumber p
    if not $ null (phoneType p)
        then putStrLn $ " - " ++ phoneType p
        else putStrLn ""
