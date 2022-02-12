module Id exposing (Id, id, match)


id : Int -> Id
id value =
    Id value


match : Id -> Id -> Bool
match a b =
    a == b


type Id
    = Id Int
