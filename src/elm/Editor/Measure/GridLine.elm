module Editor.Measure.GridLine exposing
    ( GridLine
    , initial, primary, secondary, tertiary
    , toColor
    )

{-|

@docs GridLine
@docs initial, primary, secondary, tertiary
@docs toColor

-}


type GridLine
    = InitialLine
    | PrimaryLine
    | SecondaryLine
    | TertiaryLine


initial : GridLine
initial =
    InitialLine


primary : GridLine
primary =
    PrimaryLine


secondary : GridLine
secondary =
    SecondaryLine


tertiary : GridLine
tertiary =
    TertiaryLine


toColor : GridLine -> String
toColor gridLine =
    case gridLine of
        InitialLine ->
            "#ffffff40"

        PrimaryLine ->
            "#ffffff30"

        SecondaryLine ->
            "#ffffff18"

        TertiaryLine ->
            "#ffffff0D"
