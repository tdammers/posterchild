{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module SQL
where

import Database.Posterchild

selectSightingsByUserQS :: String
selectSightingsByUserQS =
  " select sighting.timestamp, species.scientific_name " ++
  "  from user as u " ++
  "  inner join sighting as s " ++
  "    on s.user_id = u.id " ++
  "  inner join species as sp " ++
  "    on s.species_id = sp.id " ++
  "  left join species_name as spn " ++
  "    on spn.species_id = sp.id " ++
  "       and spn.language = $2" ++
  "  where u.username = $1 "

birdtrackerSchema :: Schema
birdtrackerSchema =
    Schema
      "birdtracker"
      [ ( "user"
        , Table
            { tableColumns =
                [ Column "id" SqlIntegerT NotNull
                , Column "username" SqlTextT NotNull
                , Column "password" SqlBlobT Null
                ]
            , tableConstraints =
                [
                ]
            }
        )
      , ( "species"
        , Table
            { tableColumns =
                [ Column "id" SqlIntegerT NotNull
                , Column "scientific_name" SqlTextT NotNull
                ]
            , tableConstraints =
                [
                ]
            }
        )
      , ( "species_name"
        , Table
            { tableColumns =
                [ Column "id" SqlIntegerT NotNull
                , Column "species_id" SqlIntegerT NotNull
                , Column "language" SqlTextT NotNull
                , Column "name" SqlTextT NotNull
                ]
            , tableConstraints =
                [ ForeignKeyConstraint $
                    ForeignKey
                      "species" [("species_id", "id")]
                      Cascade
                      Cascade
                ]
            }
        )
      , ( "sighting"
        , Table
            { tableColumns =
                [ Column "id" SqlIntegerT NotNull
                , Column "species_id" SqlIntegerT NotNull
                , Column "user_id" SqlIntegerT NotNull
                , Column "timestamp" (SqlTimestampWithTimeZoneT 6) NotNull
                ]
            , tableConstraints =
                [ ForeignKeyConstraint $
                    ForeignKey
                      "species" [("species_id", "id")]
                      SetNull
                      Cascade
                ]
            }
        )
      ]
