module Id exposing (Id, id, match)

import UUID


id : UUID.UUID -> Id
id value =
    Id value


match : Id -> Id -> Bool
match a b =
    a == b


type Id
    = Id UUID.UUID
