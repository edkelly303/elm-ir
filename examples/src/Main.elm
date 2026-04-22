module Main exposing (..)

import Adapters
import Exhaustive
import Fuzz
import Html
import IR exposing (IRCodec)
import Json.Decode as JD
import Json.Encode as JE


type Example
    = Yellow
    | Green String
    | Red Bool String


exampleMultitool : IRCodec Example Example
exampleMultitool =
    IR.custom
        (\red yellow green value ->
            case value of
                Red b s ->
                    red b s

                Yellow ->
                    yellow

                Green s ->
                    green s
        )
        |> IR.variant2 Red IR.bool IR.string
        |> IR.variant0 Yellow
        |> IR.variant1 Green IR.string
        |> IR.endCustom


fuzzer : Fuzz.Fuzzer Example
fuzzer =
    Adapters.fuzzer exampleMultitool


decoder : JD.Decoder Example
decoder =
    Adapters.decoder exampleMultitool


encoder : Example -> JE.Value
encoder =
    Adapters.encode exampleMultitool


exhaustive : Exhaustive.Generator Example
exhaustive =
    Adapters.exhaustive exampleMultitool



-- main


main : Html.Html msg
main =
    let
        fuzzed =
            Fuzz.examples 3 fuzzer

        encoded =
            JE.encode 2 (JE.list encoder fuzzed)

        decoded =
            JD.decodeString (JD.list decoder) encoded

        exhaustives =
            exhaustive.every 5
    in
    Html.pre []
        [ head "Fuzzer"
        , show fuzzed
        , head "JSON encoder"
        , Html.text encoded
        , head "JSON decoder"
        , show decoded
        , head "Exhaustive generator"
        , show exhaustives
        ]


head : String -> Html.Html msg
head txt =
    Html.h3 [] [ Html.text txt ]


show : a -> Html.Html msg
show a =
    Html.text (Debug.toString a)
