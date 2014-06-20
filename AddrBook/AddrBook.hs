module AddrBook.AddrBook (addrBookLoop) where

import Control.Monad.Trans (liftIO)
import Database.HDBC
import System.Exit
import Text.Parsec

import AddrBook.DeleteDB
import AddrBook.InsertDB
import AddrBook.PrintDB
import AddrBook.SelectDB
import AddrBook.Types
import AddrBook.UpdateDB

addrBookLoop :: IConnection c => c -> AddrBook
addrBookLoop con = 
    try (processRecords con) <|>
    try (printCommand con)   <|>
    try (insertUser con)     <|>
    try (insertCommand con)  <|>
    try (changeRecord con)   <|>
    try (deleteRecord con)   <|>
    processQuit

processRecords :: IConnection c => c -> AddrBook
processRecords con = do
    char ','
    selectUserIndex con
    dot <- getState
    liftIO $ printUserIndex 1 dot
    return dot

printCommand :: IConnection c => c -> AddrBook
printCommand con = do
    which <- many digit
    char 'p'
    spaces 
    sub <- oneOf "peam"
    who <- convertIndex which
    case sub of 
        'p' -> do
            selectPhones con who
            dot <- getState
            liftIO $ printPhones 1 dot
            return dot
        'e' -> do
            selectEmails con who
            dot <- getState
            liftIO $ printEmails 1 dot
            return dot
        'a' -> do
            selectAddresses con who
            dot <- getState
            liftIO $ printAddresses 1 dot
            return dot
        'm' -> do
            selectMisc con who
            dot <- getState
            liftIO $ printMisc 1 dot
            return dot

insertUser :: IConnection c => c -> AddrBook
insertUser con = do
    char 'i'
    spaces
    fName <- many $ noneOf " "
    spaces
    lName <- optionMaybe . many $ noneOf " "
    liftIO $ insertPerson con fName lName
    dot <- getState
    return dot

insertCommand :: IConnection c => c -> AddrBook
insertCommand con = do
    which <- many digit
    char 'i'
    spaces
    sub <- oneOf "peam"
    who <- convertIndex which
    dot <- getState
    case sub of
        'p' -> do
            spaces
            num <- many (noneOf " ")
            spaces
            typ <- optionMaybe (many (noneOf " "))
            liftIO $ insertPhone con num typ who
            return dot
        'e' -> do
            spaces
            addr <- many (noneOf " ")
            liftIO $ insertEmail con addr who
            return dot
        'a' -> do
            spaces
            char '"'
            addr <- many (noneOf "\"")
            char '"'
            liftIO $ insertAddress con addr who
            return dot
        'm' -> do
            spaces
            char '"'
            info <- many (noneOf "\"")
            char '"'
            liftIO $ insertMisc con info who
            return dot

changeRecord :: IConnection c => c -> AddrBook
changeRecord con = do
    which <- many digit
    char 'c'
    spaces
    sub <- oneOf "upeam"
    who <- convertIndex which
    dot <- getState
    case sub of
        'u' -> do
            spaces
            fName <- many $ noneOf " "
            spaces
            lName <- optionMaybe . many $ noneOf " "
            liftIO $ updatePerson con fName lName who
            return dot
        'p' -> do
            spaces
            num <- many $ noneOf " "
            spaces
            typ <- optionMaybe . many $ noneOf " "
            liftIO $ updatePhone con num typ who
            return dot
        'e' -> do
            spaces
            addr <- many $ noneOf " "
            liftIO $ updateEmail con addr who
            return dot
        'a' -> do
            spaces
            char '"'
            addr <- many $ noneOf "\""
            char '"'
            liftIO $ updateAddress con addr who
            return dot
        'm' -> do
            spaces
            char '"'
            info <- many $ noneOf "\""
            char '"'
            liftIO $ updateMisc con info who
            return dot

deleteRecord :: IConnection c => c -> AddrBook
deleteRecord con = do
    which <- many digit
    char 'd'
    spaces
    sub <- oneOf "upeam"
    who <- convertIndex which
    dot <- getState
    case sub of
        'u' -> liftIO $ deletePerson con who >> return dot
        'p' -> liftIO $ deletePhone con who >> return dot
        'e' -> liftIO $ deleteEmail con who >> return dot
        'a' -> liftIO $ deleteAddress con who >> return dot
        'm' -> liftIO $ deleteMisc con who >> return dot

processQuit :: AddrBook
processQuit = do
    char 'q'
    dot <- getState
    liftIO exitSuccess
    return dot

convertIndex :: String -> ParsecT String [Dot] IO String
convertIndex i = do
    dot <- getState
    let intVal = read i
    -- debug
    -- why empty list?
    liftIO . putStrLn $ show dot
    return $ getId intVal dot
  where getId 1 ((User { personId = idVal }):_) = show $ idVal
        getId 1 ((Phone { phoneId = idVal }):_) = show $ idVal
        getId 1 ((Email { emailId = idVal }):_) = show $ idVal
        getId 1 ((Address { addressId = idVal }):_) = show $ idVal
        getId 1 ((Misc { miscId = idVal }):_) = show $ idVal
        getId i (d:ds) = getId (i-1) ds
