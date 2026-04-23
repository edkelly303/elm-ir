module Main exposing (..)

import Adapters
import Fuzz
import Html
import IR exposing (Codec)
import Json.Decode as JD
import Json.Encode as JE


type Example
    = Yellow
    | Green String RecordField
    | Red Bool String


type alias RecordField =
    { field1 : Int, field2 : Char }


fieldCodec : Codec RecordField RecordField
fieldCodec =
    IR.succeed RecordField
        |> IR.andMap .field1 IR.int
        |> IR.andMap .field2 IR.char


exampleMultitool : Codec Example Example
exampleMultitool =
    IR.custom
        (\red yellow green value ->
            case value of
                Red b s ->
                    red b s

                Yellow ->
                    yellow

                Green s r ->
                    green s r
        )
        |> IR.variant2 Red IR.bool IR.string
        |> IR.variant0 Yellow
        |> IR.variant2 Green IR.string fieldCodec
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



-- there's something really slow about the exhaustive generator adapter, let's
-- switch it off for now...
--
-- exhaustive : Exhaustive.Generator Example
-- exhaustive =
--     Adapters.exhaustive exampleMultitool


main : Html.Html msg
main =
    let
        fuzzed =
            Fuzz.examples 5 fuzzer

        encoded =
            JE.encode 2 (JE.list encoder fuzzed)

        decoded =
            JD.decodeString (JD.list decoder) encoded
    in
    Html.pre []
        [ head "Fuzzer"
        , show fuzzed
        , head "JSON encoder"
        , Html.text encoded
        , head "JSON decoder"
        , show decoded

        -- there's something really slow about the exhaustive generator adapter,
        -- let's switch it off for now...
        --
        -- , head "Exhaustive generator"
        -- , show (exhaustive.nth 0)
        ]


head : String -> Html.Html msg
head txt =
    Html.h3 [] [ Html.text txt ]


show : a -> Html.Html msg
show a =
    Html.text (Debug.toString a)
