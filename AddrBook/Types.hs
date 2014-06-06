module AddrBook.Types 
    ( User(..)
    , Phone(..)
    , Email(..)
    , Address(..)
    , Misc(..)
    ) where

data User = User
    { personId  :: Int
    , firstName :: String
    , lastName  :: String }

data Phone = Phone
    { phoneId     :: Int
    , phoneNumber :: String
    , phoneType   :: String
    , phoneUid    :: Int }

data Email = Email
    { emailId   :: Int
    , emailAddr :: String
    , emailUid  :: Int }

data Address = Address
    { addressId   :: Int
    , addressAddr :: String
    , addressUid  :: Int }

data Misc = Misc
    { miscId   :: Int
    , miscInfo :: String
    , miscUid  :: Int }
