module Research.IR exposing (..)

-- example


type Semaphore
    = Yellow
    | Green Bool
    | Red Bool String


example : IR Semaphore
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
        |> variant2 Red bool string
        |> variant0 Yellow
        |> variant1 Green bool
        |> endCustom



-- innards


type IR a
    = IR
        { toIRType : a -> IRType
        }


type IRType
    = Bool Bool
    | String String
      -- | Product (List IRType)
    | Custom Int (List IRType)


bool : IR Bool
bool =
    IR { toIRType = Bool }


string : IR String
string =
    IR { toIRType = String }


custom : match -> { match : match, index : Int }
custom match =
    { match = match, index = 0 }


variant0 :
    variant
    -> { match : IRType -> match, index : Int }
    -> { match : match, index : Int }
variant0 ctor prev =
    { match = prev.match <| Custom prev.index []
    , index = prev.index + 1
    }


variant1 :
    (arg -> variant)
    -> IR arg
    -> { match : (arg -> IRType) -> match, index : Int }
    -> { match : match, index : Int }
variant1 ctor (IR argfns) prev =
    { match = prev.match <| \arg -> Custom prev.index [ argfns.toIRType arg ]
    , index = prev.index + 1
    }


variant2 :
    (arg1 -> arg2 -> variant)
    -> IR arg1
    -> IR arg2
    -> { match : (arg1 -> arg2 -> IRType) -> match, index : Int }
    -> { match : match, index : Int }
variant2 ctor (IR arg1fns) (IR arg2fns) prev =
    { match = prev.match <| \arg1 arg2 -> Custom prev.index [ arg1fns.toIRType arg1, arg2fns.toIRType arg2 ]
    , index = prev.index + 1
    }


endCustom : { match : a -> IRType, index : Int } -> IR a
endCustom prev =
    IR { toIRType = prev.match }
