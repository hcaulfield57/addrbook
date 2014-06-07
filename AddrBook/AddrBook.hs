module AddrBook.AddrBook (addrBookLoop) where

import Control.Monad.Trans (liftIO)
import Database.HDBC
import System.Exit
import Text.Parsec

import AddrBook.InsertDB
import AddrBook.PrintDB
import AddrBook.SelectDB
import AddrBook.Types

addrBookLoop :: IConnection c => c -> AddrBookMonad
addrBookLoop con = 
    try (processRecords con) <|>
    try (printCommand con)   <|>
    try (insertCommand con)  <|>
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

processQuit :: AddrBookMonad
processQuit = do
    char 'q'
    liftIO exitSuccess
