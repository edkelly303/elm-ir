module Multitool exposing
    ( bool
    , char
    , custom
    , dict
    , endCustom
    , endRecord
    , field
    , float
    , int
    , list
    , map
    , maybe
    , record
    , result
    , set
    , string
    , triple
    , tuple
    , variant0
    , variant1
    , variant2
    , variant3
    , variant4
    , variant5
    )


int =
    .int


bool =
    .bool


string =
    .string


float =
    .float


char =
    .char


list =
    .list


dict =
    .dict


set =
    .set


maybe a =
    custom
        (\nothing_ just_ variant ->
            case variant of
                Nothing ->
                    nothing_

                Just a_ ->
                    just_ a_
        )
        |> variant0 "Nothing" Nothing
        |> variant1 "Just" Just a
        |> endCustom


result x a =
    custom
        (\err_ ok_ variant ->
            case variant of
                Err x_ ->
                    err_ x_

                Ok a_ ->
                    ok_ a_
        )
        |> variant1 "Err" Err x
        |> variant1 "Ok" Ok a
        |> endCustom


tuple fst snd =
    record Tuple.pair
        |> field "First" Tuple.first fst
        |> field "Second" Tuple.second snd
        |> endRecord


triple fst snd thd =
    record (\fst_ snd_ thd_ -> ( fst_, snd_, thd_ ))
        |> field "First" (\( fst_, _, _ ) -> fst_) fst
        |> field "Second" (\( _, snd_, _ ) -> snd_) snd
        |> field "Third" (\( _, _, thd_ ) -> thd_) thd
        |> endRecord


custom match =
    \adapter -> adapter.custom match


variant0 name ctor prev =
    \adapter ->
        prev adapter
            |> adapter.variant0 name ctor


variant1 name ctor arg1 prev =
    \adapter ->
        prev adapter
            |> adapter.variant1 name ctor (arg1 adapter)


variant2 name ctor arg1 arg2 prev =
    \adapter ->
        prev adapter
            |> adapter.variant2 name ctor (arg1 adapter) (arg2 adapter)


variant3 name ctor arg1 arg2 arg3 prev =
    \adapter ->
        prev adapter
            |> adapter.variant3 name ctor (arg1 adapter) (arg2 adapter) (arg3 adapter)


variant4 name ctor arg1 arg2 arg3 arg4 prev =
    \adapter ->
        prev adapter
            |> adapter.variant4 name ctor (arg1 adapter) (arg2 adapter) (arg3 adapter) (arg4 adapter)


variant5 name ctor arg1 arg2 arg3 arg4 arg5 prev =
    \adapter ->
        prev adapter
            |> adapter.variant5 name ctor (arg1 adapter) (arg2 adapter) (arg3 adapter) (arg4 adapter) (arg5 adapter)


endCustom prev =
    \adapter ->
        prev adapter
            |> adapter.endCustom


record ctor =
    \adapter -> adapter.record ctor


field name getter arg prev =
    \adapter ->
        prev adapter
            |> adapter.field name getter (arg adapter)


endRecord prev =
    \adapter ->
        prev adapter
            |> adapter.endRecord


map f prev =
    \adapter ->
        prev adapter
            |> adapter.map f
