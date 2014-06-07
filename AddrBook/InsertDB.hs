{-# LANGUAGE FlexibleContexts #-}

module AddrBook.InsertDB
    ( insertPerson
    , insertPhone
    , insertEmail
    , insertAddress
    , insertMisc
    , maybeSql
    ) where

import Data.Convertible
import Database.HDBC

insertPerson :: IConnection c => c -> String -> Maybe String -> IO ()
insertPerson con fName lName = do
    let firstName = toSql fName
        lastName  = maybeSql lName
    run con ("insert into Person (PersonId, FirstName, LastName) " ++
                  "values (null, ?, ?); ")
        [firstName, lastName]
    commit con

insertPhone :: IConnection c => c -> String -> Maybe String -> Char -> IO ()
insertPhone con phoneN phoneT pid = do
    let phoneNumber = toSql phoneN
        phoneType   = maybeSql phoneT
        personId    = toSql pid
    run con ("insert into Phone (PhoneId, PhoneNumber, PhoneType, PersonId) " ++
                  "values (null, ?, ?, ?); ")
        [phoneNumber, phoneType, personId]
    commit con

insertEmail :: IConnection c => c -> String -> Char -> IO ()
insertEmail con emailAddr pid = do
    let emailAddress = toSql emailAddr
        personId     = toSql pid
    run con ("insert into Email (EmailId, EmailAddress, PersonId) " ++
                  "values (null, ?, ?); ")
        [emailAddress, personId]
    commit con

insertAddress :: IConnection c => c -> String -> Char -> IO ()
insertAddress con addressAddr pid = do
    let addressAddress = toSql addressAddr
        personId       = toSql pid
    run con ("insert into Address (AddressId, AddressAddress, PersonId) " ++
                  "values (null, ?, ?); ")
        [addressAddress, personId]
    commit con

insertMisc :: IConnection c => c -> String -> Char -> IO ()
insertMisc con misc pid = do
    let miscInfo = toSql misc
        personId = toSql pid
    run con ("insert into Misc (MiscId, MiscInfo, PersonId) " ++
                  "values (null, ?, ?); ")
        [miscInfo, personId]
    commit con

maybeSql :: Convertible a SqlValue => Maybe a -> SqlValue 
maybeSql (Just a) = toSql a
maybeSql Nothing  = SqlNull
