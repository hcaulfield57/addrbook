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
    liftIO $ printUserIndex dot

printCommand :: IConnection c => c -> AddrBookMonad
printCommand con = do
    which <- digit
    char 'p'
    spaces 
    sub <- oneOf "peam"
    case sub of 
        'p' -> do
            selectPhones con which
            dot <- getState
            liftIO $ printPhones dot
        'e' -> do
            selectEmails con which
            dot <- getState
            liftIO $ printEmails dot
        'a' -> do
            selectAddresses con which
            dot <- getState
            liftIO $ printAddresses dot
        'm' -> do
            selectMisc con which
            dot <- getState
            liftIO $ printMisc dot

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
    which <- digit
    char 'i'
    spaces
    sub <- oneOf "peam"
    case sub of
        'p' -> do
            spaces
            num <- many (noneOf " ")
            spaces
            typ <- optionMaybe (many (noneOf " "))
            liftIO $ insertPhone con num typ which
        'e' -> do
            spaces
            addr <- many (noneOf " ")
            liftIO $ insertEmail con addr which
        'a' -> do
            spaces
            char '"'
            addr <- many (noneOf "\"")
            char '"'
            liftIO $ insertAddress con addr which
        'm' -> do
            spaces
            char '"'
            info <- many (noneOf "\"")
            char '"'
            liftIO $ insertMisc con info which

changeRecord :: IConnection c => c -> AddrBookMonad
changeRecord con = do
    which <- digit
    char 'c'
    spaces
    sub <- oneOf "upeam"
    case sub of
        'u' -> do
            spaces
            fName <- many $ noneOf " "
            spaces
            lName <- optionMaybe . many $ noneOf " "
            liftIO $ updatePerson con fName lName which
        'p' -> do
            spaces
            num <- many $ noneOf " "
            spaces
            typ <- optionMaybe . many $ noneOf " "
            liftIO $ updatePhone con num typ which
        'e' -> do
            spaces
            addr <- many $ noneOf " "
            liftIO $ updateEmail con addr which
        'a' -> do
            spaces
            char '"'
            addr <- many $ noneOf "\""
            char '"'
            liftIO $ updateAddress con addr which
        'm' -> do
            spaces
            char '"'
            info <- many $ noneOf "\""
            char '"'
            liftIO $ updateMisc con info which

deleteRecord :: IConnection c => c -> AddrBookMonad
deleteRecord con = do
    which <- digit
    char 'd'
    spaces
    sub <- oneOf "upeam"
    case sub of
        'u' -> liftIO $ deletePerson con which
        'p' -> liftIO $ deletePhone con which
        'e' -> liftIO $ deleteEmail con which
        'a' -> liftIO $ deleteAddress con which
        'm' -> liftIO $ deleteMisc con which

processQuit :: AddrBookMonad
processQuit = do
    char 'q'
    liftIO exitSuccess
