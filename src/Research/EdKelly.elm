module Research.EdKelly exposing
    ( CustomToValueMaker
    , Semaphore(..)
    , ToValue
    , Value
    , bool
    , buildCustom
    , custom
    , example
    , string
    , variant0
    , variant1
    , variant2
    )

import NestedTuple as NT


type Semaphore
    = Red Bool String
    | Yellow
    | Green Bool


example : Semaphore -> Semaphore -> Maybe Value
example =
    custom
        (\red yellow green value ->
            case value of
                Red i s ->
                    red i s

                Yellow ->
                    yellow

                Green f ->
                    green f
        )
        |> variant2 bool string
        |> variant0
        |> variant1 bool
        |> buildCustom


type Value
    = CustomValue Int (List Value)
    | Value Int


type alias ToValue a =
    a -> Value


string : ToValue String
string =
    always (Value 1)


bool : ToValue Bool
bool =
    always (Value 2)


type CustomToValueMaker match empty getters setters extractorMaker doer
    = CustomToValueMaker
        { match : match
        , empty : empty
        , getters : getters
        , setters : setters
        , extractorMaker : extractorMaker
        , doer : doer
        , index : Int
        }


custom match =
    CustomToValueMaker
        { match = match
        , empty = NT.define
        , getters = NT.defineGetters
        , setters = NT.defineSetters
        , extractorMaker = NT.define
        , doer = NT.define
        , index = 0
        }


variant0 (CustomToValueMaker previousCustomToValueMaker) =
    CustomToValueMaker
        { match = previousCustomToValueMaker.match
        , empty = NT.appender Nothing previousCustomToValueMaker.empty
        , getters = NT.getter previousCustomToValueMaker.getters
        , setters = NT.setter previousCustomToValueMaker.setters
        , extractorMaker =
            NT.folder
                (\setter { empty, match } ->
                    { empty = empty
                    , match = match (setter (Just ()) empty)
                    }
                )
                previousCustomToValueMaker.extractorMaker
        , doer =
            NT.folder
                (\getter { fst, snd, out } ->
                    let
                        newOut =
                            case ( getter fst, getter snd ) of
                                ( Just (), _ ) ->
                                    Just
                                        (CustomValue previousCustomToValueMaker.index
                                            []
                                        )

                                _ ->
                                    out
                    in
                    { fst = fst, snd = snd, out = newOut }
                )
                previousCustomToValueMaker.doer
        , index = previousCustomToValueMaker.index + 1
        }


variant1 argToValue (CustomToValueMaker previousCustomToValueMaker) =
    CustomToValueMaker
        { match = previousCustomToValueMaker.match
        , empty = NT.appender Nothing previousCustomToValueMaker.empty
        , getters = NT.getter previousCustomToValueMaker.getters
        , setters = NT.setter previousCustomToValueMaker.setters
        , extractorMaker =
            NT.folder
                (\setter { empty, match } ->
                    { empty = empty
                    , match = match (\arg -> setter (Just arg) empty)
                    }
                )
                previousCustomToValueMaker.extractorMaker
        , doer =
            NT.folder
                (\getter { fst, snd, out } ->
                    let
                        newOut =
                            case ( getter fst, getter snd ) of
                                ( Just fArg, Just sArg ) ->
                                    Just
                                        (CustomValue previousCustomToValueMaker.index
                                            [ argToValue fArg
                                            , argToValue sArg
                                            ]
                                        )

                                ( Just fArg, Nothing ) ->
                                    Just
                                        (CustomValue previousCustomToValueMaker.index
                                            [ argToValue fArg
                                            ]
                                        )

                                _ ->
                                    out
                    in
                    { fst = fst, snd = snd, out = newOut }
                )
                previousCustomToValueMaker.doer
        , index = previousCustomToValueMaker.index + 1
        }


variant2 arg1ToValue arg2ToValue (CustomToValueMaker previousCustomToValueMaker) =
    CustomToValueMaker
        { match = previousCustomToValueMaker.match
        , empty = NT.appender Nothing previousCustomToValueMaker.empty
        , getters = NT.getter previousCustomToValueMaker.getters
        , setters = NT.setter previousCustomToValueMaker.setters
        , extractorMaker =
            NT.folder
                (\setter { empty, match } ->
                    { empty = empty
                    , match = match (\arg1 arg2 -> setter (Just ( arg1, arg2 )) empty)
                    }
                )
                previousCustomToValueMaker.extractorMaker
        , doer =
            NT.folder
                (\getter { fst, snd, out } ->
                    let
                        newOut =
                            case ( getter fst, getter snd ) of
                                ( Just ( fArg1, fArg2 ), Just ( sArg1, sArg2 ) ) ->
                                    Just
                                        (CustomValue previousCustomToValueMaker.index
                                            [ arg1ToValue fArg1
                                            , arg2ToValue fArg2
                                            , arg1ToValue sArg1
                                            , arg2ToValue sArg2
                                            ]
                                        )

                                ( Just ( fArg1, fArg2 ), Nothing ) ->
                                    Just
                                        (CustomValue previousCustomToValueMaker.index
                                            [ arg1ToValue fArg1
                                            , arg2ToValue fArg2
                                            ]
                                        )

                                _ ->
                                    out
                    in
                    { fst = fst, snd = snd, out = newOut }
                )
                previousCustomToValueMaker.doer
        , index = previousCustomToValueMaker.index + 1
        }


buildCustom (CustomToValueMaker prev) =
    let
        empty =
            NT.endAppender prev.empty

        getters =
            NT.endGetters prev.getters

        setters =
            NT.endSetters prev.setters

        makeExtractor =
            NT.endFolder prev.extractorMaker

        extractor =
            makeExtractor
                { empty = empty, match = prev.match }
                setters
                |> .match

        doer =
            NT.endFolder prev.doer
    in
    \fst snd ->
        doer
            { fst = extractor fst
            , snd = extractor snd
            , out = Nothing
            }
            getters
            |> .out
