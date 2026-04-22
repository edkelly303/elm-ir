module Research.IR exposing (..)

import Fuzz
import Json.Decode as JD
import Json.Encode as JE



-- example


type Semaphore
    = Yellow
    | Green Bool String
    | Red Bool Bool


semaphoreCodec : IRCodec Semaphore Semaphore
semaphoreCodec =
    custom
        (\red yellow green value ->
            case value of
                Red i s ->
                    red i s

                Yellow ->
                    yellow

                Green f x ->
                    green f x
        )
        |> variant2 Red bool bool
        |> variant0 Yellow
        |> variant2 Green bool string
        |> endCustom


unitCodec : IRCodec () ()
unitCodec =
    semaphoreCodec
        |> contramap (\() -> Yellow)
        |> map (\_ -> Yellow)
        |> andThen
            (\s ->
                case s of
                    Yellow ->
                        Ok ()

                    _ ->
                        Err Error
            )


encodeSemaphore : Semaphore -> JE.Value
encodeSemaphore =
    encode semaphoreCodec


semaphoreDecoder : JD.Decoder Semaphore
semaphoreDecoder =
    decoder semaphoreCodec


unitEncoder : () -> JE.Value
unitEncoder =
    encode unitCodec


unitDecoder : JD.Decoder ()
unitDecoder =
    decoder unitCodec


encode : IRCodec a b -> a -> JE.Value
encode codec value =
    codec.toIR value
        |> encodeAdapter


decoder : IRCodec a b -> JD.Decoder b
decoder codec =
    decodeAdapter
        |> JD.andThen
            (\ir ->
                case codec.fromIR ir of
                    Ok s ->
                        JD.succeed s

                    Err Error ->
                        JD.fail ""
            )


fuzzer : IRCodec a b -> Fuzz.Fuzzer IR
fuzzer codec =
    codec.toIRType
        |> fuzzAdapter



-- adapters


fuzzAdapter : IRType -> Fuzz.Fuzzer IR
fuzzAdapter irType =
    case irType of
        BoolT ->
            Fuzz.bool |> Fuzz.map Bool

        StringT ->
            Fuzz.string |> Fuzz.map String

        CustomT variants ->
            Fuzz.oneOf
                (List.indexedMap
                    (\idx variant ->
                        case variant of
                            Variant0T ->
                                Fuzz.constant
                                    (Custom idx Variant0)

                            Variant1T arg ->
                                Fuzz.map
                                    (\a -> Custom idx (Variant1 a))
                                    (fuzzAdapter arg)

                            Variant2T arg1 arg2 ->
                                Fuzz.map2
                                    (\a1 a2 -> Custom idx (Variant2 a1 a2))
                                    (fuzzAdapter arg1)
                                    (fuzzAdapter arg2)
                    )
                    variants
                )


encodeAdapter : IR -> JE.Value
encodeAdapter irType =
    case irType of
        Bool b ->
            JE.object
                [ ( "bool", JE.bool b ) ]

        String s ->
            JE.object
                [ ( "string", JE.string s ) ]

        Custom selected variant ->
            JE.object
                [ ( "custom"
                  , JE.object
                        [ ( "tag", JE.int selected )
                        , ( "args"
                          , JE.list encodeAdapter
                                (case variant of
                                    Variant0 ->
                                        []

                                    Variant1 arg ->
                                        [ arg ]

                                    Variant2 arg1 arg2 ->
                                        [ arg1
                                        , arg2
                                        ]
                                )
                          )
                        ]
                  )
                ]


decodeAdapter : JD.Decoder IR
decodeAdapter =
    JD.oneOf
        [ JD.field "bool" JD.bool |> JD.map Bool
        , JD.field "string" JD.string |> JD.map String
        , JD.field "custom"
            (JD.map2
                (\selected args ->
                    case args of
                        [] ->
                            Just (Custom selected Variant0)

                        [ arg ] ->
                            Just (Custom selected (Variant1 arg))

                        [ arg1, arg2 ] ->
                            Just (Custom selected (Variant2 arg1 arg2))

                        _ ->
                            Nothing
                )
                (JD.field "tag" JD.int)
                (JD.field "args" (JD.list (JD.lazy (\_ -> decodeAdapter))))
                |> JD.andThen
                    (\mc ->
                        case mc of
                            Nothing ->
                                JD.fail ""

                            Just c ->
                                JD.succeed c
                    )
            )
        ]



-- innards


type Error
    = Error


type alias IRCodec a b =
    { toIR : a -> IR
    , fromIR : IR -> Result Error b
    , toIRType : IRType
    }


type IR
    = Bool Bool
    | String String
    | Custom Int Variant


type Variant
    = Variant0
    | Variant1 IR
    | Variant2 IR IR


type IRType
    = BoolT
    | StringT
    | CustomT (List VariantT)


type VariantT
    = Variant0T
    | Variant1T IRType
    | Variant2T IRType IRType


bool : IRCodec Bool Bool
bool =
    { toIR = Bool
    , fromIR =
        \ir ->
            case ir of
                Bool b ->
                    Ok b

                _ ->
                    Err Error
    , toIRType = BoolT
    }


string : IRCodec String String
string =
    { toIR = String
    , fromIR =
        \ir ->
            case ir of
                String s ->
                    Ok s

                _ ->
                    Err Error
    , toIRType = StringT
    }


custom match =
    { match = match
    , index = 0
    , fromIR = \_ -> Err Error
    , toIRType = []
    }


variant0 ctor prev =
    { match = prev.match <| Custom prev.index Variant0
    , index = prev.index + 1
    , fromIR =
        \ir ->
            case ir of
                Custom selected Variant0 ->
                    if selected == prev.index then
                        Ok ctor

                    else
                        prev.fromIR ir

                _ ->
                    prev.fromIR ir
    , toIRType = Variant0T :: prev.toIRType
    }


variant1 ctor argfns prev =
    { match = prev.match <| \arg -> Custom prev.index (Variant1 (argfns.toIR arg))
    , index = prev.index + 1
    , fromIR =
        \ir ->
            case ir of
                Custom selected (Variant1 arg) ->
                    if selected == prev.index then
                        Result.map ctor (argfns.fromIR arg)

                    else
                        prev.fromIR ir

                _ ->
                    prev.fromIR ir
    , toIRType = Variant1T argfns.toIRType :: prev.toIRType
    }


variant2 ctor arg1fns arg2fns prev =
    { match = prev.match <| \arg1 arg2 -> Custom prev.index (Variant2 (arg1fns.toIR arg1) (arg2fns.toIR arg2))
    , index = prev.index + 1
    , fromIR =
        \ir ->
            case ir of
                Custom selected (Variant2 arg1 arg2) ->
                    if selected == prev.index then
                        Result.map2 ctor (arg1fns.fromIR arg1) (arg2fns.fromIR arg2)

                    else
                        prev.fromIR ir

                _ ->
                    prev.fromIR ir
    , toIRType = Variant2T arg1fns.toIRType arg2fns.toIRType :: prev.toIRType
    }


endCustom prev =
    { toIR = prev.match
    , fromIR = prev.fromIR
    , toIRType = CustomT prev.toIRType
    }


map :
    (b -> c)
    -> IRCodec a b
    -> IRCodec a c
map f prev =
    { toIR = prev.toIR
    , fromIR = prev.fromIR >> Result.map f
    , toIRType = prev.toIRType
    }


contramap :
    (b -> a)
    -> IRCodec a c
    -> IRCodec b c
contramap f prev =
    { toIR = f >> prev.toIR
    , fromIR = prev.fromIR
    , toIRType = prev.toIRType
    }


andThen :
    (b -> Result Error c)
    -> IRCodec a b
    -> IRCodec a c
andThen f prev =
    { toIR = prev.toIR
    , fromIR = prev.fromIR >> Result.andThen f
    , toIRType = prev.toIRType
    }
