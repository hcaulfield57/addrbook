module AddrBook.SelectDB (selectAll, selectInfo) where

import Database.HDBC

selectAll :: IConnection c => c -> IO [[SqlValue]]
selectAll con = quickQuery' con 
    ("select PersonId, FirstName, LastName " ++
       "from Person; ") []

selectInfo :: IConnection c => c -> Int -> IO [[SqlValue]]
selectInfo con num = quickQuery' con
    ("select P.FirstName "              ++
          ", P.LastName "               ++
          ", Ph.PhoneNumber "           ++
          ", E.EmailAddress "           ++
          ", A.AddressAddress "         ++
          ", M.MiscInfo "               ++
       "from Person P "                 ++
       "join Phone Ph "                 ++
         "on P.PersonId = Ph.PersonId " ++
       "join Email E "                  ++
         "on P.PersonId = E.PersonId "  ++
       "join Address A "                ++
         "on P.PersonId = A.PersonId "  ++
       "join Misc M "                   ++
         "on P.PersonId = M.PersonId "  ++
      "where P.PersonId = ?; ")
    [toSql num]
