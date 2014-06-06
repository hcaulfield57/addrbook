module AddrBook.SelectDB 
    ( selectUserIndex
    , selectPhones
    ) where

import Database.HDBC

import AddrBook.PrintDB
import AddrBook.Types

selectUserIndex :: IConnection c => c -> IO [User]
selectUserIndex con = do
    res <- quickQuery' con
        ("select PersonId "  ++
              ", FirstName " ++
              ", LastName "  ++
           "from Person; ")
        []
    return $ getUsers res

getUsers :: [[SqlValue]] -> [User]
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

selectPhones :: IConnection c => c -> Char -> Maybe Char -> IO ()
selectPhones con start Nothing = do
    res <- quickQuery' con
        ("select Ph.PhoneId "               ++
              ", Ph.PhoneNumber "           ++
              ", Ph.PhoneType "             ++
           "from Phone Ph "                ++
           "join Person P "                 ++
             "on Ph.PersonId = P.PersonId " ++
          "where P.PersonId = ?; ")
        [toSql start]
    printPhones $ getPhones res

getPhones :: [[SqlValue]] -> [Phone]
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
