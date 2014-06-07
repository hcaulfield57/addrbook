module AddrBook.DeleteDB
    ( deletePerson
    , deletePhone
    , deleteEmail
    , deleteAddress
    , deleteMisc
    ) where

import Database.HDBC

deletePerson :: IConnection c => c -> Char -> IO ()
deletePerson con which = do
    let who = toSql which
    run con ("delete from Person " ++
                   "where PersonId = ?; ")
        [who]
    commit con

deletePhone :: IConnection c => c -> Char -> IO ()
deletePhone con which = do
    let who = toSql which
    run con ("delete from Phone " ++
                   "where PhoneId = ?; ")
        [who]
    commit con

deleteEmail :: IConnection c => c -> Char -> IO ()
deleteEmail con which = do
    let who = toSql which
    run con ("delete from Email " ++
                   "where EmailId = ?; ")
        [who]
    commit con

deleteAddress :: IConnection c => c -> Char -> IO ()
deleteAddress con which = do
    let who = toSql which
    run con ("delete from Address " ++
                   "where AddressId = ?; ")
        [who]
    commit con

deleteMisc :: IConnection c => c -> Char -> IO ()
deleteMisc con which = do
    let who = toSql which
    run con ("delete from Misc " ++
                   "where MiscId = ?; ")
        [who]
    commit con
