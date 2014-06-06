module AddrBook.AddrBook (addrBookLoop) where

import Control.Monad.Trans (liftIO)
import Database.HDBC
import System.Exit
import Text.Parsec

import AddrBook.PrintDB
import AddrBook.SelectDB
import AddrBook.Types

type AddrBookMonad = ParsecT String [User] IO ()

addrBookLoop :: IConnection c => c -> AddrBookMonad
addrBookLoop con = 
    try (processRecords con) <|>
    try (printCommand con)   <|>
    processQuit

processRecords :: IConnection c => c -> AddrBookMonad
processRecords con = do
    char ','
    users <- liftIO $ selectUserIndex con
    putState users
    liftIO $ printUserIndex users

-- Printing Subcommands

printCommand :: IConnection c => c -> AddrBookMonad
printCommand con = do
    start <- digit
    end   <- optionMaybe range
    char 'p'
    spaces 
    sub <- oneOf "p"
    case sub of 
        'p' -> liftIO $ selectPhones con start end

range :: ParsecT String [User] IO Char
range = char ',' >> digit

processQuit :: AddrBookMonad
processQuit = do
    char 'q'
    liftIO exitSuccess
