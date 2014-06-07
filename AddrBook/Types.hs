module AddrBook.Types (AddrBookMonad, Dot(..)) where

import Text.Parsec

-- Basic monad for the program.
type AddrBookMonad = ParsecT String [Dot] IO ()

-- Basic 'Dot' type, this provides the current state for the program, to
-- determine what records a command addresses. Each constructor refers to
-- a specific table in the database.
data Dot 
    = User
    { personId  :: Int
    , firstName :: String
    , lastName  :: String }

    | Phone
    { phoneId     :: Int
    , phoneNumber :: String
    , phoneType   :: String
    , personId    :: Int }

    | Email
    { emailId   :: Int
    , emailAddr :: String
    , personId  :: Int }

    | Address
    { addressId   :: Int
    , addressAddr :: String
    , personId    :: Int }

    | Misc
    { miscId   :: Int
    , miscInfo :: String
    , personId :: Int }
