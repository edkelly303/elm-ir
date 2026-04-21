module Multitool exposing (..)


int =
    .int


bool =
    .bool


string =
    .string


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


andThen f prev =
    \adapter ->
        prev adapter
            |> adapter.andThen (\v -> f adapter v)
