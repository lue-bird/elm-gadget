module IR exposing
    ( Codec
    , bool, char, string, int, float
    , list, array, dict, set
    , tuple, triple
    , RecordCodecBuilder, record, field, endRecord
    , maybe, result
    , CustomCodecBuilder, custom
    , variant0, variant1, variant2, variant3, variant4, variant5
    , endCustom
    , map
    , label
    )

{-|


# elm-ir

Convert between Elm data types and an intermediate representation (IR)


## IR codecs

@docs Codec


## IR primitives

@docs bool, char, string, int, float


## IR combinators


### Common data types


#### Collections

@docs list, array, dict, set


### Product types

@docs tuple, triple
@docs RecordCodecBuilder, record, field, endRecord


### Custom types

@docs maybe, result
@docs CustomCodecBuilder, custom
@docs variant0, variant1, variant2, variant3, variant4, variant5
@docs endCustom


### Transforming codec input and output

@docs map


### Labelling codecs

@docs label

-}

import Array
import Dict
import IR.Adapter exposing (Codec(..), Error, IR(..), IRType(..), Variant(..), VariantType(..))
import Result.Extra
import Set


{-| TODO
-}
type alias Codec a =
    IR.Adapter.Codec a


{-| TODO
-}
type RecordCodecBuilder input output
    = RecordCodecBuilder
        { fromInput : input -> List IR
        , toOutput : List IR -> Result Error output
        , irType : List IRType
        }


{-| TODO
-}
type CustomCodecBuilder input hasAtLeastOneVariant output
    = CustomCodec
        { match : input
        , fromIR : IR -> Result Error output
        , variantTypes : List VariantType
        , index : Int
        }


{-| TODO
-}
bool : Codec Bool
bool =
    Codec
        { fromInput = Bool
        , toOutput =
            \ir ->
                case ir of
                    Bool b ->
                        Ok b

                    _ ->
                        Err "bool toOutput failed"
        , irType = BoolType
        }


{-| TODO
-}
char : Codec Char
char =
    Codec
        { fromInput = Char
        , toOutput =
            \ir ->
                case ir of
                    Char c ->
                        Ok c

                    _ ->
                        Err "char toOutput failed"
        , irType = CharType
        }


{-| TODO
-}
string : Codec String
string =
    Codec
        { fromInput = String
        , toOutput =
            \ir ->
                case ir of
                    String s ->
                        Ok s

                    _ ->
                        Err "string toOutput failed"
        , irType = StringType
        }


{-| TODO
-}
int : Codec Int
int =
    Codec
        { fromInput = Int
        , toOutput =
            \ir ->
                case ir of
                    Int i ->
                        Ok i

                    _ ->
                        Err "int toOutput failed"
        , irType = IntType
        }


{-| TODO
-}
float : Codec Float
float =
    Codec
        { fromInput = Float
        , toOutput =
            \ir ->
                case ir of
                    Float s ->
                        Ok s

                    _ ->
                        Err "float toOutput failed"
        , irType = FloatType
        }


{-| TODO
-}
list : Codec a -> Codec (List a)
list (Codec item) =
    Codec
        { fromInput = \items -> List (List.map item.fromInput items)
        , toOutput =
            \ir ->
                case ir of
                    List items ->
                        List.map item.toOutput items
                            |> Result.Extra.combine

                    _ ->
                        Err "list toOutput failed"
        , irType = ListType item.irType
        }


{-| TODO
-}
dict :
    Codec comparable
    -> Codec v
    -> Codec (Dict.Dict comparable v)
dict key value =
    list (tuple key value)
        |> map Dict.fromList Dict.toList


{-| TODO
-}
set :
    Codec comparable
    -> Codec (Set.Set comparable)
set value =
    list value
        |> map Set.fromList Set.toList


{-| TODO
-}
array : Codec a -> Codec (Array.Array a)
array item =
    list item
        |> map Array.fromList Array.toList


{-| TODO
-}
maybe : Codec a -> Codec (Maybe a)
maybe item =
    custom
        (\just nothing variant ->
            case variant of
                Just a ->
                    just a

                Nothing ->
                    nothing
        )
        |> variant1 Just item
        |> variant0 Nothing
        |> endCustom


{-| TODO
-}
result : Codec x -> Codec a -> Codec (Result x a)
result x a =
    custom
        (\err ok variant ->
            case variant of
                Err x_ ->
                    err x_

                Ok a_ ->
                    ok a_
        )
        |> variant1 Err x
        |> variant1 Ok a
        |> endCustom


{-| TODO
-}
tuple : Codec a -> Codec b -> Codec ( a, b )
tuple a b =
    record Tuple.pair
        |> field Tuple.first a
        |> field Tuple.second b
        |> endRecord


{-| TODO
-}
triple : Codec a -> Codec b -> Codec c -> Codec ( a, b, c )
triple a b c =
    record (\a_ b_ c_ -> ( a_, b_, c_ ))
        |> field (\( a_, _, _ ) -> a_) a
        |> field (\( _, b_, _ ) -> b_) b
        |> field (\( _, _, c_ ) -> c_) c
        |> endRecord


{-| TODO
-}
custom : input -> CustomCodecBuilder input Never output
custom match =
    CustomCodec
        { match = match
        , index = 0
        , fromIR = \_ -> Err "custom toOutput failed"
        , variantTypes = []
        }


{-| TODO
-}
variant0 :
    output
    -> CustomCodecBuilder (IR -> input) variantType output
    -> CustomCodecBuilder input () output
variant0 ctor (CustomCodec prev) =
    CustomCodec
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
        , variantTypes =
            Variant0Type
                :: prev.variantTypes
        }


{-| TODO
-}
variant1 :
    (arg1 -> output)
    -> Codec arg1
    -> CustomCodecBuilder ((arg1 -> IR) -> input) variantType output
    -> CustomCodecBuilder input () output
variant1 ctor (Codec argfns) (CustomCodec prev) =
    CustomCodec
        { match = prev.match <| \arg -> Custom prev.index (Variant1 (argfns.fromInput arg))
        , index = prev.index + 1
        , fromIR =
            \ir ->
                case ir of
                    Custom selected (Variant1 arg) ->
                        if selected == prev.index then
                            Result.map ctor (argfns.toOutput arg)

                        else
                            prev.fromIR ir

                    _ ->
                        prev.fromIR ir
        , variantTypes =
            Variant1Type argfns.irType
                :: prev.variantTypes
        }


{-| TODO
-}
variant2 :
    (arg1 -> arg2 -> output)
    -> Codec arg1
    -> Codec arg2
    -> CustomCodecBuilder ((arg1 -> arg2 -> IR) -> input) variantType output
    -> CustomCodecBuilder input () output
variant2 ctor (Codec arg1fns) (Codec arg2fns) (CustomCodec prev) =
    CustomCodec
        { match = prev.match <| \arg1 arg2 -> Custom prev.index (Variant2 (arg1fns.fromInput arg1) (arg2fns.fromInput arg2))
        , index = prev.index + 1
        , fromIR =
            \ir ->
                case ir of
                    Custom selected (Variant2 arg1 arg2) ->
                        if selected == prev.index then
                            Result.map2 ctor (arg1fns.toOutput arg1) (arg2fns.toOutput arg2)

                        else
                            prev.fromIR ir

                    _ ->
                        prev.fromIR ir
        , variantTypes =
            Variant2Type arg1fns.irType arg2fns.irType
                :: prev.variantTypes
        }


{-| TODO
-}
variant3 :
    (arg1 -> arg2 -> arg3 -> output)
    -> Codec arg1
    -> Codec arg2
    -> Codec arg3
    -> CustomCodecBuilder ((arg1 -> arg2 -> arg3 -> IR) -> input) variantType output
    -> CustomCodecBuilder input () output
variant3 ctor (Codec arg1fns) (Codec arg2fns) (Codec arg3fns) (CustomCodec prev) =
    CustomCodec
        { match =
            prev.match <|
                \arg1 arg2 arg3 ->
                    Custom prev.index
                        (Variant3
                            (arg1fns.fromInput arg1)
                            (arg2fns.fromInput arg2)
                            (arg3fns.fromInput arg3)
                        )
        , index = prev.index + 1
        , fromIR =
            \ir ->
                case ir of
                    Custom selected (Variant3 arg1 arg2 arg3) ->
                        if selected == prev.index then
                            Result.map3 ctor
                                (arg1fns.toOutput arg1)
                                (arg2fns.toOutput arg2)
                                (arg3fns.toOutput arg3)

                        else
                            prev.fromIR ir

                    _ ->
                        prev.fromIR ir
        , variantTypes =
            Variant3Type
                arg1fns.irType
                arg2fns.irType
                arg3fns.irType
                :: prev.variantTypes
        }


{-| TODO
-}
variant4 :
    (arg1 -> arg2 -> arg3 -> arg4 -> output)
    -> Codec arg1
    -> Codec arg2
    -> Codec arg3
    -> Codec arg4
    -> CustomCodecBuilder ((arg1 -> arg2 -> arg3 -> arg4 -> IR) -> input) variantType output
    -> CustomCodecBuilder input () output
variant4 ctor (Codec arg1fns) (Codec arg2fns) (Codec arg3fns) (Codec arg4fns) (CustomCodec prev) =
    CustomCodec
        { match =
            prev.match <|
                \arg1 arg2 arg3 arg4 ->
                    Custom prev.index
                        (Variant4
                            (arg1fns.fromInput arg1)
                            (arg2fns.fromInput arg2)
                            (arg3fns.fromInput arg3)
                            (arg4fns.fromInput arg4)
                        )
        , index = prev.index + 1
        , fromIR =
            \ir ->
                case ir of
                    Custom selected (Variant4 arg1 arg2 arg3 arg4) ->
                        if selected == prev.index then
                            Result.map4 ctor
                                (arg1fns.toOutput arg1)
                                (arg2fns.toOutput arg2)
                                (arg3fns.toOutput arg3)
                                (arg4fns.toOutput arg4)

                        else
                            prev.fromIR ir

                    _ ->
                        prev.fromIR ir
        , variantTypes =
            Variant4Type
                arg1fns.irType
                arg2fns.irType
                arg3fns.irType
                arg4fns.irType
                :: prev.variantTypes
        }


{-| TODO
-}
variant5 :
    (arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> output)
    -> Codec arg1
    -> Codec arg2
    -> Codec arg3
    -> Codec arg4
    -> Codec arg5
    -> CustomCodecBuilder ((arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> IR) -> input) variantType output
    -> CustomCodecBuilder input () output
variant5 ctor (Codec arg1fns) (Codec arg2fns) (Codec arg3fns) (Codec arg4fns) (Codec arg5fns) (CustomCodec prev) =
    CustomCodec
        { match =
            prev.match <|
                \arg1 arg2 arg3 arg4 arg5 ->
                    Custom prev.index
                        (Variant5
                            (arg1fns.fromInput arg1)
                            (arg2fns.fromInput arg2)
                            (arg3fns.fromInput arg3)
                            (arg4fns.fromInput arg4)
                            (arg5fns.fromInput arg5)
                        )
        , index = prev.index + 1
        , fromIR =
            \ir ->
                case ir of
                    Custom selected (Variant5 arg1 arg2 arg3 arg4 arg5) ->
                        if selected == prev.index then
                            Result.map5 ctor
                                (arg1fns.toOutput arg1)
                                (arg2fns.toOutput arg2)
                                (arg3fns.toOutput arg3)
                                (arg4fns.toOutput arg4)
                                (arg5fns.toOutput arg5)

                        else
                            prev.fromIR ir

                    _ ->
                        prev.fromIR ir
        , variantTypes =
            Variant5Type
                arg1fns.irType
                arg2fns.irType
                arg3fns.irType
                arg4fns.irType
                arg5fns.irType
                :: prev.variantTypes
        }


{-| TODO
-}
endCustom : CustomCodecBuilder (a -> IR) () a -> Codec a
endCustom (CustomCodec prev) =
    Codec
        { fromInput = prev.match
        , toOutput = prev.fromIR
        , irType =
            case List.reverse prev.variantTypes of
                [] ->
                    -- we know this can't happen, because if the second type
                    -- variable of CustomCodec is `()`, then we know that we've
                    -- used at least one `variantX` function, so the list of
                    -- variants can't be empty. So it's ok to use a spurious
                    -- Variant0Type here, because this will never get produced.
                    CustomType Variant0Type []

                firstVariantType :: restVariantTypes ->
                    CustomType firstVariantType restVariantTypes
        }


{-| TODO
-}
record : output -> RecordCodecBuilder input output
record ctor =
    RecordCodecBuilder
        { fromInput = \_ -> []
        , toOutput = \_ -> Ok ctor
        , irType = []
        }


{-| TODO
-}
field :
    (input -> field)
    -> Codec field
    -> RecordCodecBuilder input (field -> output)
    -> RecordCodecBuilder input output
field getter (Codec codec) (RecordCodecBuilder builder) =
    RecordCodecBuilder
        { fromInput =
            \input ->
                let
                    thisField =
                        codec.fromInput (getter input)

                    prevFields =
                        builder.fromInput input
                in
                thisField :: prevFields
        , toOutput =
            \fields ->
                case fields of
                    thisField :: prevFields ->
                        Result.map2 (\ctor val -> ctor val)
                            (builder.toOutput prevFields)
                            (codec.toOutput thisField)

                    [] ->
                        Err "andMap toOutput failed"
        , irType =
            codec.irType :: builder.irType
        }


{-| TODO
-}
endRecord : RecordCodecBuilder a a -> Codec a
endRecord (RecordCodecBuilder builder) =
    Codec
        { fromInput =
            \input ->
                Product (List.reverse (builder.fromInput input))
        , toOutput =
            \ir ->
                case ir of
                    Product fields ->
                        builder.toOutput (List.reverse fields)

                    _ ->
                        Err ""
        , irType = ProductType (List.reverse builder.irType)
        }


{-| TODO
-}
map :
    (a -> b)
    -> (b -> a)
    -> Codec a
    -> Codec b
map aToB bToA (Codec prev) =
    Codec
        { fromInput = bToA >> prev.fromInput
        , toOutput = prev.toOutput >> Result.map aToB
        , irType = prev.irType
        }


{-| TODO
-}
label : String -> Codec a -> Codec a
label label_ (Codec c) =
    Codec
        { fromInput =
            \input ->
                case c.fromInput input of
                    Labelled labels inner ->
                        Labelled (Set.insert label_ labels) inner

                    other ->
                        Labelled (Set.singleton label_) other
        , toOutput =
            \ir ->
                case ir of
                    Labelled _ innerIR ->
                        c.toOutput innerIR

                    _ ->
                        c.toOutput ir
        , irType =
            case c.irType of
                LabelledType labels inner ->
                    LabelledType (Set.insert label_ labels) inner

                other ->
                    LabelledType (Set.singleton label_) other
        }
