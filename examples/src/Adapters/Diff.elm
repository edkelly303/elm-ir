module Adapters.Diff exposing (..)

import Dict
import IR
import List.Extra
import Maybe.Extra


type Diff
    = Changes (List ( Int, Diff ))
    | CustomChanges Int (List ( Int, Diff ))
    | BoolChange Bool
    | IntChange Int
    | FloatChange Float
    | CharChange Char
    | StringChange String
      -- | DictChange DictChanges
      -- | SetChange SetChanges
    | ListChange (List ListChange)


type ListChange
    = Added Diff
    | Moved Int
    | Updated Int Diff
    | Existing Int Int


diff : IR.Codec output output -> output -> output -> Diff
diff codec old new =
    let
        oldIR =
            IR.fromInput codec old

        newIR =
            IR.fromInput codec new

        help oldIR_ newIR_ =
            case ( oldIR_, newIR_ ) of
                ( IR.Bool b1, IR.Bool b2 ) ->
                    if b1 == b2 then
                        Changes []

                    else
                        BoolChange b2

                ( IR.String b1, IR.String b2 ) ->
                    if b1 == b2 then
                        Changes []

                    else
                        StringChange b2

                ( IR.Char b1, IR.Char b2 ) ->
                    if b1 == b2 then
                        Changes []

                    else
                        CharChange b2

                ( IR.Float b1, IR.Float b2 ) ->
                    if b1 == b2 then
                        Changes []

                    else
                        FloatChange b2

                ( IR.Int b1, IR.Int b2 ) ->
                    if b1 == b2 then
                        Changes []

                    else
                        IntChange b2

                ( IR.Product fields1, IR.Product fields2 ) ->
                    if List.length fields1 == List.length fields2 then
                        List.map2 help fields1 fields2
                            |> List.indexedMap Tuple.pair
                            |> Changes

                    else
                        Changes []

                ( IR.Custom oldSelected oldVariant, IR.Custom newSelected newVariant ) ->
                    let
                        argsToList variant =
                            case variant of
                                IR.Variant0 ->
                                    []

                                IR.Variant1 a ->
                                    [ a ]

                                IR.Variant2 a1 a2 ->
                                    [ a1, a2 ]

                        newArgs =
                            argsToList newVariant
                    in
                    if oldSelected == newSelected then
                        let
                            oldArgs =
                                argsToList oldVariant

                            diffedArgs =
                                List.Extra.zip oldArgs newArgs
                                    |> List.indexedMap
                                        (\idx ( oldArg, newArg ) ->
                                            ( idx, help oldArg newArg )
                                        )
                                    |> List.filter (\( _, arg ) -> arg /= Changes [])
                        in
                        CustomChanges newSelected diffedArgs

                    else
                        let
                            diffedArgs =
                                newArgs
                                    |> List.indexedMap (\idx newArg -> ( idx, help (default newArg) newArg ))
                                    |> List.filter (\( _, arg ) -> arg /= Changes [])
                        in
                        CustomChanges newSelected diffedArgs

                _ ->
                    Changes []
    in
    help oldIR newIR


default : IR.IR -> IR.IR
default irType =
    case irType of
        IR.Bool _ ->
            IR.Bool True

        IR.Char _ ->
            IR.Char ' '

        IR.String _ ->
            IR.String ""

        IR.Int _ ->
            IR.Int 0

        IR.Float _ ->
            IR.Float 0.0

        IR.Custom selected variant ->
            IR.Custom selected
                (case variant of
                    IR.Variant0 ->
                        IR.Variant0

                    IR.Variant1 arg ->
                        IR.Variant1 (default arg)

                    IR.Variant2 arg1 arg2 ->
                        IR.Variant2 (default arg1) (default arg2)
                )

        IR.Product fields ->
            IR.Product (List.map default fields)

        IR.List _ ->
            IR.List []


defaultFromType : IR.IRType -> IR.IR
defaultFromType irType =
    case irType of
        IR.BoolType ->
            IR.Bool True

        IR.CharType ->
            IR.Char ' '

        IR.StringType ->
            IR.String ""

        IR.IntType ->
            IR.Int 0

        IR.FloatType ->
            IR.Float 0.0

        IR.CustomType firstVariantType _ ->
            IR.Custom 0
                (case firstVariantType of
                    IR.Variant0Type ->
                        IR.Variant0

                    IR.Variant1Type arg ->
                        IR.Variant1 (defaultFromType arg)

                    IR.Variant2Type arg1 arg2 ->
                        IR.Variant2 (defaultFromType arg1) (defaultFromType arg2)
                )

        IR.ProductType fieldTypes ->
            IR.Product (List.map defaultFromType fieldTypes)

        IR.ListType _ ->
            IR.List []


patch : IR.Codec a a -> Diff -> a -> Result IR.Error a
patch codec delta old =
    let
        help changes_ old_ irType_ =
            case ( changes_, old_, irType_ ) of
                ( Changes [], any, _ ) ->
                    Just any

                ( BoolChange b, IR.Bool _, _ ) ->
                    Just (IR.Bool b)

                ( CharChange b, IR.Char _, _ ) ->
                    Just (IR.Char b)

                ( StringChange b, IR.String _, _ ) ->
                    Just (IR.String b)

                ( IntChange b, IR.Int _, _ ) ->
                    Just (IR.Int b)

                ( FloatChange b, IR.Int _, _ ) ->
                    Just (IR.Float b)

                ( Changes fieldChanges, IR.Product oldFields, IR.ProductType fieldTypes ) ->
                    let
                        fieldChangesDict =
                            Dict.fromList fieldChanges
                    in
                    List.Extra.zip oldFields fieldTypes
                        |> List.indexedMap
                            (\idx ( oldField, fieldType ) ->
                                case Dict.get idx fieldChangesDict of
                                    Nothing ->
                                        Just oldField

                                    Just change ->
                                        help change oldField fieldType
                            )
                        |> Maybe.Extra.combine
                        |> Maybe.map IR.Product

                ( CustomChanges diffSelected diffVariant, IR.Custom oldSelected oldVariant, IR.CustomType firstVariantType restVariantTypes ) ->
                    let
                        argsDict =
                            Dict.fromList diffVariant
                    in
                    List.Extra.getAt diffSelected (firstVariantType :: restVariantTypes)
                        |> Maybe.andThen
                            (\variantType ->
                                case variantType of
                                    IR.Variant0Type ->
                                        Just IR.Variant0

                                    IR.Variant1Type argType ->
                                        if diffSelected == oldSelected then
                                            case oldVariant of
                                                IR.Variant1 arg ->
                                                    case Dict.get 0 argsDict of
                                                        Nothing ->
                                                            Just (IR.Variant1 arg)

                                                        Just changes ->
                                                            Maybe.map IR.Variant1 (help changes arg argType)

                                                _ ->
                                                    Nothing

                                        else
                                            case Dict.get 0 argsDict of
                                                Nothing ->
                                                    Just (IR.Variant1 (defaultFromType argType))

                                                Just changes ->
                                                    Maybe.map IR.Variant1 (help changes (defaultFromType argType) argType)

                                    IR.Variant2Type arg1Type arg2Type ->
                                        if diffSelected == oldSelected then
                                            case oldVariant of
                                                IR.Variant2 arg1 arg2 ->
                                                    let
                                                        arg1Diff =
                                                            case Dict.get 0 argsDict of
                                                                Nothing ->
                                                                    Just arg1

                                                                Just changes ->
                                                                    help changes arg1 arg1Type

                                                        arg2Diff =
                                                            case Dict.get 1 argsDict of
                                                                Nothing ->
                                                                    Just arg2

                                                                Just changes ->
                                                                    help changes arg2 arg2Type
                                                    in
                                                    Maybe.map2 IR.Variant2 arg1Diff arg2Diff

                                                _ ->
                                                    Nothing

                                        else
                                            let
                                                arg1Diff =
                                                    case Dict.get 0 argsDict of
                                                        Nothing ->
                                                            Just (defaultFromType arg1Type)

                                                        Just changes ->
                                                            help changes (defaultFromType arg1Type) arg1Type

                                                arg2Diff =
                                                    case Dict.get 1 argsDict of
                                                        Nothing ->
                                                            Just (defaultFromType arg2Type)

                                                        Just changes ->
                                                            help changes (defaultFromType arg2Type) arg2Type
                                            in
                                            Maybe.map2 IR.Variant2 arg1Diff arg2Diff
                            )
                        |> Maybe.map (IR.Custom diffSelected)

                _ ->
                    Nothing

        oldIR =
            IR.fromInput codec old

        irType =
            IR.irType codec

        maybeNewIR =
            help delta oldIR irType
    in
    case maybeNewIR of
        Just ir ->
            IR.toOutput codec ir

        Nothing ->
            Err IR.Error
