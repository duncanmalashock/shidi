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


view : Int -> Int -> String
view width height =
    List.indexedMap (viewKey width height) list
        |> String.join ""


viewKey : Int -> Int -> Int -> Key -> String
viewKey width height index key =
    """<rect x="0" y="$y" width="$width" height="$height" fill="$fill"></rect>"""
        |> String.replace "$y" (String.fromInt (height * index))
        |> String.replace "$height" (String.fromInt height)
        |> String.replace "$width" (String.fromInt width)
        |> String.replace "$fill" (toFillColor key)


toFillColor : Key -> String
toFillColor key =
    case key of
        WhiteKey ->
            "#191921"

        BlackKey ->
            "#0c0e14"
