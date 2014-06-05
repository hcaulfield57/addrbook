module AddrBook.CreateDB (createDB) where

import Database.HDBC

-- table creations have been split up into separate
-- calls to run for readability
createDB :: IConnection c => c -> IO ()
createDB con = do
    -- Person
    runRaw con
        ("create table Person "                             ++
           "( PersonId  integer primary key autoincrement " ++
           ", FirstName text not null "                     ++
           ", LastName  text); ")
    -- Phone
    runRaw con
        ("create table Phone "                                ++ 
           "( PhoneId     integer primary key autoincrement " ++
           ", PhoneNumber text not null "                     ++
           ", PhoneType   text "                              ++
           ", PersonId    integer not null "                  ++
           ", foreign key (PersonId) references Person (PersonId)); ")
    -- Email
    runRaw con
        ("create table Email "                                 ++
           "( EmailId      integer primary key autoincrement " ++
           ", EmailAddress text not null "                     ++
           ", PersonId     integer not null "                  ++
           ", foreign key (PersonId) references Person (PersonId)); ")
    -- Address
    runRaw con
        ("create table Address "                                 ++
           "( AddressId      integer primary key autoincrement " ++
           ", AddressAddress text not null "                     ++
           ", PersonId       integer not null "                  ++
           ", foreign key (PersonId) references Person (PersonId)); ")
    -- Misc
    runRaw con
        ("create table Misc "                              ++
           "( MiscId   integer primary key autoincrement " ++
           ", MiscInfo text not null "                     ++
           ", PersonId integer not null "                  ++
           ", foreign key (PersonId) references Person (PersonId)); ")
    -- don't forget!
    commit con
