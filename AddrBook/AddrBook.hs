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

addrBookLoop :: IConnection c => c -> AddrBookMonad
addrBookLoop con = 
    try (processRecords con) <|>
    try (printCommand con)   <|>
    try (insertUser con)     <|>
    try (insertCommand con)  <|>
    try (changeRecord con)   <|>
    try (deleteRecord con)   <|>
    processQuit

processRecords :: IConnection c => c -> AddrBookMonad
processRecords con = do
    char ','
    selectUserIndex con
    dot <- getState
    liftIO $ printUserIndex 1 dot

printCommand :: IConnection c => c -> AddrBookMonad
printCommand con = do
    which <- many digit
    char 'p'
    spaces 
    sub <- oneOf "peam"
    case sub of 
        'p' -> do
            selectPhones con which
            dot <- getState
            liftIO $ printPhones 1 dot
        'e' -> do
            selectEmails con which
            dot <- getState
            liftIO $ printEmails 1 dot
        'a' -> do
            selectAddresses con which
            dot <- getState
            liftIO $ printAddresses 1 dot
        'm' -> do
            selectMisc con which
            dot <- getState
            liftIO $ printMisc 1 dot

insertUser :: IConnection c => c -> AddrBookMonad
insertUser con = do
    char 'i'
    spaces
    fName <- many $ noneOf " "
    spaces
    lName <- optionMaybe . many $ noneOf " "
    liftIO $ insertPerson con fName lName

insertCommand :: IConnection c => c -> AddrBookMonad
insertCommand con = do
    which <- many digit
    char 'i'
    spaces
    sub <- oneOf "peam"
    who <- convertIndex which
    case sub of
        'p' -> do
            spaces
            num <- many (noneOf " ")
            spaces
            typ <- optionMaybe (many (noneOf " "))
            liftIO $ insertPhone con num typ who
        'e' -> do
            spaces
            addr <- many (noneOf " ")
            liftIO $ insertEmail con addr who
        'a' -> do
            spaces
            char '"'
            addr <- many (noneOf "\"")
            char '"'
            liftIO $ insertAddress con addr who
        'm' -> do
            spaces
            char '"'
            info <- many (noneOf "\"")
            char '"'
            liftIO $ insertMisc con info who

changeRecord :: IConnection c => c -> AddrBookMonad
changeRecord con = do
    which <- many digit
    char 'c'
    spaces
    sub <- oneOf "upeam"
    who <- convertIndex which
    case sub of
        'u' -> do
            spaces
            fName <- many $ noneOf " "
            spaces
            lName <- optionMaybe . many $ noneOf " "
            liftIO $ updatePerson con fName lName who
        'p' -> do
            spaces
            num <- many $ noneOf " "
            spaces
            typ <- optionMaybe . many $ noneOf " "
            liftIO $ updatePhone con num typ who
        'e' -> do
            spaces
            addr <- many $ noneOf " "
            liftIO $ updateEmail con addr who
        'a' -> do
            spaces
            char '"'
            addr <- many $ noneOf "\""
            char '"'
            liftIO $ updateAddress con addr who
        'm' -> do
            spaces
            char '"'
            info <- many $ noneOf "\""
            char '"'
            liftIO $ updateMisc con info who

deleteRecord :: IConnection c => c -> AddrBookMonad
deleteRecord con = do
    which <- many digit
    char 'd'
    spaces
    sub <- oneOf "upeam"
    who <- convertIndex which
    case sub of
        'u' -> liftIO $ deletePerson con who
        'p' -> liftIO $ deletePhone con who
        'e' -> liftIO $ deleteEmail con who
        'a' -> liftIO $ deleteAddress con who
        'm' -> liftIO $ deleteMisc con who

processQuit :: AddrBookMonad
processQuit = do
    char 'q'
    liftIO exitSuccess

convertIndex :: String -> ParsecT String [Dot] IO String
convertIndex i = do
    dot <- getState
    let intVal = read i
    return $ getId intVal dot
  where getId 1 ((User { personId = idVal }):_) = show $ idVal
        getId 1 ((Phone { phoneId = idVal }):_) = show $ idVal
        getId 1 ((Email { emailId = idVal }):_) = show $ idVal
        getId 1 ((Address { addressId = idVal }):_) = show $ idVal
        getId 1 ((Misc { miscId = idVal }):_) = show $ idVal
        getId i (d:ds) = getId (i-1) ds
