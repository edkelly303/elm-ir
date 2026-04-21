module Research.MiniBill exposing
    ( CustomToValueMaker
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


type Semaphore
    = Red Bool String
    | Yellow
    | Green Bool


example : Semaphore -> Value
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


type CustomToValueMaker match v
    = CustomToValueMaker
        { match : match
        , index : Int
        }


custom : match -> CustomToValueMaker match a
custom match =
    CustomToValueMaker
        { match = match
        , index = 0
        }


variant0 :
    CustomToValueMaker (Value -> match) a
    -> CustomToValueMaker match a
variant0 (CustomToValueMaker previousCustomToValueMaker) =
    CustomToValueMaker
        { match =
            previousCustomToValueMaker.match <|
                CustomValue previousCustomToValueMaker.index
                    []
        , index = previousCustomToValueMaker.index + 1
        }


variant1 :
    ToValue arg
    -> CustomToValueMaker ((arg -> Value) -> match) a
    -> CustomToValueMaker match a
variant1 argToValue (CustomToValueMaker previousCustomToValueMaker) =
    CustomToValueMaker
        { match =
            previousCustomToValueMaker.match <|
                \arg ->
                    CustomValue previousCustomToValueMaker.index
                        [ argToValue arg ]
        , index = previousCustomToValueMaker.index + 1
        }


variant2 :
    ToValue arg1
    -> ToValue arg2
    -> CustomToValueMaker ((arg1 -> arg2 -> Value) -> match) a
    -> CustomToValueMaker match a
variant2 arg1ToValue arg2ToValue (CustomToValueMaker previousCustomToValueMaker) =
    CustomToValueMaker
        { match =
            previousCustomToValueMaker.match <|
                \arg1 arg2 ->
                    CustomValue previousCustomToValueMaker.index
                        [ arg1ToValue arg1
                        , arg2ToValue arg2
                        ]
        , index = previousCustomToValueMaker.index + 1
        }


buildCustom : CustomToValueMaker (a -> Value) a -> ToValue a
buildCustom (CustomToValueMaker prev) =
    \variant -> prev.match variant
