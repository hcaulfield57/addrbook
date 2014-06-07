module AddrBook.AddrBook (addrBookLoop) where

import Control.Monad.Trans (liftIO)
import Database.HDBC
import System.Exit
import Text.Parsec

import AddrBook.PrintDB
import AddrBook.SelectDB
import AddrBook.Types

addrBookLoop :: IConnection c => c -> AddrBookMonad
addrBookLoop con = 
    try (processRecords con) <|>
    try (printCommand con)   <|>
    processQuit

processRecords :: IConnection c => c -> AddrBookMonad
processRecords con = do
    char ','
    selectUserIndex con
    dot <- getState
    liftIO $ printUserIndex dot

-- Printing Subcommands

printCommand :: IConnection c => c -> AddrBookMonad
printCommand con = do
    which <- digit
    char 'p'
    spaces 
    sub <- oneOf "pea"
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

range :: ParsecT String [Dot] IO Char
range = char ',' >> digit

processQuit :: AddrBookMonad
processQuit = do
    char 'q'
    liftIO exitSuccess
