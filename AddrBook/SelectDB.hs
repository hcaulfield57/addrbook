module AddrBook.SelectDB 
    ( selectUserIndex
    , selectPhones
    , selectEmails
    , selectAddresses
    , selectMisc
    ) where

import Control.Monad.Trans (liftIO)
import Database.HDBC
import Text.Parsec

import AddrBook.PrintDB
import AddrBook.Types

-- Users

selectUserIndex :: IConnection c => c -> AddrBookMonad
selectUserIndex con = do
    res <- liftIO $ quickQuery' con
        ("select PersonId "  ++
              ", FirstName " ++
              ", LastName "  ++
           "from Person; ")
        []
    let users = getUsers res
    putState users

getUsers :: [[SqlValue]] -> [Dot]
getUsers [] = []
getUsers ((i:f:[]):us) =
    let userid = fromSql i
        fname  = fromSql f
        lname  = ""
    in User userid fname lname : getUsers us
getUsers ((i:f:l:[]):us) = 
    let userid = fromSql i
        fname  = fromSql f
        lname  = fromSql l
    in User userid fname lname : getUsers us

-- Phones

selectPhones :: IConnection c => c -> Char -> AddrBookMonad
selectPhones con start = do
    res <- liftIO $ quickQuery' con
        ("select Ph.PhoneId "               ++
              ", Ph.PhoneNumber "           ++
              ", Ph.PhoneType "             ++
              ", Ph.PersonId "              ++
           "from Phone Ph "                 ++
           "join Person P "                 ++
             "on Ph.PersonId = P.PersonId " ++
          "where P.PersonId = ?; ")
        [toSql start]
    let phones = getPhones res
    putState phones

getPhones :: [[SqlValue]] -> [Dot]
getPhones [] = []
getPhones ((i:n:u:[]):ps) = 
    let phoneId = fromSql i
        number  = fromSql n
        user    = fromSql u
    in Phone phoneId number "" user : getPhones ps
getPhones ((i:n:t:u:[]):ps) = 
    let phoneId = fromSql i
        number  = fromSql n
        phoneT  = fromSql t
        user    = fromSql u
    in Phone phoneId number phoneT user : getPhones ps

-- Emails

selectEmails :: IConnection c => c -> Char -> AddrBookMonad
selectEmails con which = do
    res <- liftIO $ quickQuery' con
        ("select E.EmailId "               ++
              ", E.EmailAddress "          ++
              ", E.PersonId "              ++
           "from Email E "                 ++
           "join Person P "                ++
             "on E.PersonId = P.PersonId " ++
          "where P.PersonId = ?; ")
        [toSql which]
    let emails = getEmails res
    putState emails

getEmails :: [[SqlValue]] -> [Dot]
getEmails [] = []
getEmails ((i:a:u:[]):es) =
    let eid  = fromSql i
        addr = fromSql a
        user = fromSql u
    in Email eid addr user : getEmails es

-- Addresses

selectAddresses :: IConnection c => c -> Char -> AddrBookMonad
selectAddresses con which = do
    res <- liftIO $ quickQuery' con
        ("select A.AddressId "             ++
              ", A.AddressAddress "        ++
              ", A.PersonId "              ++
           "from Address A "               ++
           "join Person P "                ++
             "on A.PersonId = P.PersonId " ++
          "where P.PersonId = ?; ")
        [toSql which]
    let addresses = getAddresses res
    putState addresses

getAddresses :: [[SqlValue]] -> [Dot]
getAddresses [] = []
getAddresses ((i:a:u:[]):as) =
    let aid  = fromSql i
        addr = fromSql a
        user = fromSql u
    in Address aid addr user : getAddresses as

-- Misc

selectMisc :: IConnection c => c -> Char -> AddrBookMonad
selectMisc con which = do
    res <- liftIO $ quickQuery' con
        ("select M.MiscId "                ++
              ", M.MiscInfo "              ++
              ", M.PersonId "              ++
           "from Misc M "                  ++
           "join Person P "                ++
             "on M.PersonId = P.PersonId " ++
          "where P.PersonId = ?; ")
        [toSql which]
    let misc = getMisc res
    putState misc

getMisc :: [[SqlValue]] -> [Dot]
getMisc [] = []
getMisc ((i:m:u:[]):ms) =
    let mid  = fromSql i
        info = fromSql m
        user = fromSql u
    in Misc mid info user : getMisc ms
