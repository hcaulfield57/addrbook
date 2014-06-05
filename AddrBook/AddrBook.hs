module AddrBook.AddrBook (addrBookLoop) where

import Control.Monad.Trans (liftIO)
import Database.HDBC
import System.Exit
import Text.Parsec

import AddrBook.Print
import AddrBook.SelectDB

type AddrBookMonad = ParsecT String [[SqlValue]] IO ()

addrBookLoop :: IConnection c => c -> AddrBookMonad
addrBookLoop con = 
    try (processRecords con) <|>
    try (addressCommand con) <|>
    processQuit

processRecords :: IConnection c => c -> AddrBookMonad
processRecords con = do
    char ','
    res <- liftIO $ selectAll con
    putState res
    liftIO $ printAll res

addressCommand :: IConnection c => c -> AddrBookMonad
addressCommand con = do
    num     <- digit
    command <- letter
    case command of
        'p' -> printInfo num

processQuit :: AddrBookMonad
processQuit = do
    char 'q'
    liftIO exitSuccess
