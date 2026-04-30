module IRTest exposing (..)

import Expect
import IR
import IR.Fuzz
import Test exposing (..)
import TestHelpers exposing (..)


irTests : Test
irTests =
    Test.describe "IR"
        [ roundTrip recordCodec "Record"
        , roundTrip IR.int "Int"
        , roundTrip (IR.list IR.bool) "List Bool"
        ]


roundTrip : IR.Codec input input -> String -> Test
roundTrip codec name =
    fuzz (IR.Fuzz.fuzzer codec) (name ++ " fromInput -> toOutput roundtrip") <|
        \rec ->
            rec
                |> IR.fromInput codec
                |> IR.toOutput codec
                |> Expect.equal (Ok rec)
