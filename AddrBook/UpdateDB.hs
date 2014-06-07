module AddrBook.UpdateDB
    ( updatePerson
    , updatePhone
    , updateEmail
    , updateAddress
    , updateMisc
    ) where

import Database.HDBC

import AddrBook.InsertDB (maybeSql)

updatePerson :: IConnection c => c -> String -> Maybe String -> Char -> IO ()
updatePerson con fname lname which = do
    let firstName = toSql fname
        lastName  = maybeSql lname
        who       = toSql which
    run con ("update Person "        ++
                "set FirstName = ? " ++
                  ", LastName  = ? " ++
              "where PersonId = ?; ")
        [firstName, lastName, who]
    commit con

updatePhone :: IConnection c => c -> String -> Maybe String -> Char -> IO ()
updatePhone con num typ which = do
    let number = toSql num
        phoneT = maybeSql typ
        who    = toSql which
    run con ("update Phone "           ++
                "set PhoneNumber = ? " ++
                  ", PhoneType = ? "   ++
              "where PhoneId = ?; ")
        [number, phoneT, who]
    commit con

updateEmail :: IConnection c => c -> String -> Char -> IO ()
updateEmail con eaddr which = do
    let emailAddr = toSql eaddr
        who       = toSql which
    run con ("update Email "            ++
                "set EmailAddress = ? " ++
              "where EmailId = ?; ")
        [emailAddr, who]
    commit con

updateAddress :: IConnection c => c -> String -> Char -> IO ()
updateAddress con addr which = do
    let addrA = toSql addr
        who   = toSql which
    run con ("update Address "            ++
                "set AddressAddress = ? " ++
              "where AddressId = ?; ")
        [addrA, who]
    commit con

updateMisc :: IConnection c => c -> String -> Char -> IO ()
updateMisc con info which = do
    let miscInfo = toSql info
        who      = toSql which
    run con ("update Misc "         ++
                "set MiscInfo = ? " ++
              "where MiscId = ?; ")
        [miscInfo, who]
    commit con
