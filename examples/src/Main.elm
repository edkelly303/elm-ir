module Main exposing (..)

import Adapters
import Exhaustive
import Fuzz
import Html
import Json.Decode as JD
import Json.Encode as JE
import Multitool exposing (..)



-- example


type Example
    = Yellow
    | Green { g : Int }
    | Red Bool String


exampleMultitool =
    custom
        (\red yellow green value ->
            case value of
                Red b s ->
                    red b s

                Yellow ->
                    yellow

                Green i ->
                    green i
        )
        |> variant2 "Red" Red bool string
        |> variant0 "Yellow" Yellow
        |> variant1 "Green"
            Green
            (record (\g -> { g = g })
                |> field "g" .g int
                |> endRecord
            )
        |> endCustom





-- fuzzer : Fuzz.Fuzzer Example


fuzzer =
    exampleMultitool Adapters.fuzzAdapter



-- decoder : JD.Decoder Example


decoder =
    exampleMultitool Adapters.decoderAdapter



-- encoder : Example -> JE.Value


encoder =
    exampleMultitool Adapters.encoderAdapter



-- exhaustive : Exhaustive.Generator Example


exhaustive =
    exampleMultitool Adapters.exhaustiveAdapter



-- main


main =
    let
        fuzzed =
            Fuzz.examples 1 fuzzer

        encoded =
            JE.encode 2 (JE.list encoder fuzzed)
    in
    Html.div []
        [ Html.h3 [] [ Html.text "Fuzzer" ]
        , show fuzzed
        , Html.h3 [] [ Html.text "Codec (to JSON)" ]
        , Html.text encoded
        , Html.h3 [] [ Html.text "Codec (from JSON)" ]
        , show (JD.decodeString (JD.list decoder) encoded)
        , Html.h3 [] [ Html.text "Exhaustive generator" ]
        , show (exhaustive.nth 0)
        ]


show a =
    Html.text (Debug.toString a)
