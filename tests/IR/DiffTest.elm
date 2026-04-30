module IR.DiffTest exposing (..)

import Expect
import IR
import IR.Diff
import IR.Fuzz
import Test exposing (..)
import TestHelpers exposing (..)


diffTests : Test
diffTests =
    Test.describe "IR.Diff"
        [ roundTrip recordCodec "Record"
        , roundTrip IR.int "Int"
        , roundTrip (IR.list IR.bool) "List Bool"
        ]


roundTrip : IR.Codec b b -> String -> Test
roundTrip codec name =
    fuzz2
        (IR.Fuzz.fuzzer codec)
        (IR.Fuzz.fuzzer codec)
        (name ++ " diff -> patch roundtrip")
    <|
        \old new ->
            let
                diff =
                    IR.Diff.diff codec old new
            in
            IR.Diff.patch codec diff old
                |> Expect.equal (Ok new)
