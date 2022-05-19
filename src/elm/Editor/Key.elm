module Editor.Key exposing (Key, view)


type Key
    = WhiteKey
    | BlackKey


list : List Key
list =
    [ WhiteKey
    , BlackKey
    , WhiteKey
    , BlackKey
    , WhiteKey
    , BlackKey
    , WhiteKey
    , WhiteKey
    , BlackKey
    , WhiteKey
    , BlackKey
    , WhiteKey
    ]


view : Int -> String
view height =
    List.indexedMap (viewKey height) list
        |> String.join ""


viewKey : Int -> Int -> Key -> String
viewKey height index key =
    """<rect x="0" y="$y" width="100" height="$height" fill="$fill"></rect>"""
        |> String.replace "$y" (String.fromInt (height * index))
        |> String.replace "$height" (String.fromInt height)
        |> String.replace "$fill" (toFillColor key)


toFillColor : Key -> String
toFillColor key =
    case key of
        WhiteKey ->
            "#191921"

        BlackKey ->
            "#0c0e14"
