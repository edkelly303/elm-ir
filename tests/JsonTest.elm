module JsonTest exposing (..)

import Expect
import IR
import IR.Fuzz
import IR.Json
import Json.Decode
import Test exposing (..)
import TestHelpers exposing (..)


diffTests : Test
diffTests =
    Test.describe "IR.Json"
        [ roundTrip recordCodec "Record"
        , roundTrip IR.int "Int"
        , roundTrip (IR.list IR.bool) "List Bool"
        ]


roundTrip : IR.Codec b b -> String -> Test
roundTrip codec name =
    fuzz
        (IR.Fuzz.fuzzer codec)
        (name ++ " encode -> decode roundtrip")
    <|
        \value ->
            let
                encoded =
                    IR.Json.encode codec value
            in
            Json.Decode.decodeValue (IR.Json.decoder codec) encoded
                |> Expect.equal (Ok value)
