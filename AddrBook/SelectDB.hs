module AddrBook.SelectDB 
    ( selectUserIndex
    , selectPhones
    ) where

import Control.Monad.Trans (liftIO)
import Database.HDBC
import Text.Parsec

import AddrBook.PrintDB
import AddrBook.Types

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
