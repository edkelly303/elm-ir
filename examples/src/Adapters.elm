module Adapters exposing (..)

import Dict
import Exhaustive
import Fuzz
import Json.Decode as JD
import Json.Encode as JE


decoderAdapter =
    { bool = JD.bool
    , string = JD.string
    , int = JD.int
    , custom = \_ -> Dict.empty
    , variant0 = \name ctor prev -> Dict.insert name (JD.succeed ctor) prev
    , variant1 =
        \name ctor dec1 prev ->
            Dict.insert name
                (JD.map ctor
                    (JD.index 0 dec1)
                )
                prev
    , variant2 =
        \name ctor dec1 dec2 prev ->
            Dict.insert name
                (JD.map2 ctor
                    (JD.index 0 dec1)
                    (JD.index 1 dec2)
                )
                prev
    , endCustom =
        \prev ->
            JD.field "tag" JD.string
                |> JD.andThen
                    (\tag ->
                        case Dict.get tag prev of
                            Nothing ->
                                JD.fail <| "tag " ++ tag ++ " did not match"

                            Just dec ->
                                JD.field "args" dec
                    )
    , record = JD.succeed
    , field = \name _ dec prev -> JD.map2 (|>) (JD.field name dec) prev
    , endRecord = Basics.identity
    , map = JD.map
    }


encoderAdapter =
    { bool = JE.bool
    , string = JE.string
    , int = JE.int
    , custom = \match -> match
    , variant0 =
        \name _ prev ->
            prev
                (JE.object
                    [ ( "tag", JE.string name )
                    , ( "args", JE.null )
                    ]
                )
    , variant1 =
        \name _ enc1 prev ->
            prev <|
                \arg1 ->
                    JE.object
                        [ ( "tag", JE.string name )
                        , ( "args", JE.list identity [ enc1 arg1 ] )
                        ]
    , variant2 =
        \name _ enc1 enc2 prev ->
            prev <|
                \arg1 arg2 ->
                    JE.object
                        [ ( "tag", JE.string name )
                        , ( "args", JE.list identity [ enc1 arg1, enc2 arg2 ] )
                        ]
    , endCustom = \prev -> prev
    , record = \_ -> []
    , field = \name getter enc prev -> \rec -> ( name, enc (getter rec) ) :: prev
    , endRecord = \prev -> \rec -> JE.object (List.reverse (prev rec))
    , map = \_ x -> x
    }


fuzzAdapter =
    { bool = Fuzz.bool
    , string = Fuzz.string
    , int = Fuzz.int
    , custom = \_ -> []
    , variant0 = \_ ctor prev -> Fuzz.constant ctor :: prev
    , variant1 = \_ ctor fuzzer1 prev -> Fuzz.map ctor fuzzer1 :: prev
    , variant2 = \_ ctor fuzzer1 fuzzer2 prev -> Fuzz.map2 ctor fuzzer1 fuzzer2 :: prev
    , endCustom = \prev -> Fuzz.oneOf prev
    , record = Fuzz.constant
    , field = \_ _ f -> Fuzz.andMap f
    , endRecord = Basics.identity
    , map = Fuzz.map
    }


exhaustiveAdapter =
    { string = Exhaustive.string
    , bool = Exhaustive.bool
    , int = Exhaustive.int
    , custom = \_ -> Exhaustive.customType
    , variant0 = \_ -> Exhaustive.variant0
    , variant1 = \_ -> Exhaustive.variant1
    , variant2 = \_ -> Exhaustive.variant2
    , endCustom = Basics.identity
    , record = Exhaustive.record
    , field = \_ _ e -> Exhaustive.field e
    , endRecord = Basics.identity
    , map = Exhaustive.map
    }
