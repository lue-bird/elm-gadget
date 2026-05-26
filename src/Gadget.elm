module Gadget exposing
    ( Gadget
    , bool, char, string, int, float
    , list, array, dict, set
    , tuple, triple
    , RecordGadgetBuilder, record, field, endRecord
    , maybe, result
    , CustomGadgetBuilder, custom, variant0, variant1, variant2, variant3, variant4, variant5, endCustom
    , map
    , label
    )

{-| This module is for application developers who want to create Gadgets and use
them with pre-existing adapters.

If you want to develop your own adapters, see the [Gadget.IR](Gadget-IR) module.


# Gadgets

@docs Gadget


# Primitives

@docs bool, char, string, int, float


# Combinators


## Collections

@docs list, array, dict, set


## Product types

@docs tuple, triple

@docs RecordGadgetBuilder, record, field, endRecord


## Custom types

@docs maybe, result

@docs CustomGadgetBuilder, custom, variant0, variant1, variant2, variant3, variant4, variant5, endCustom


# Transforming input and output

@docs map


# Labels

@docs label

-}

import Array
import Dict
import Gadget.IR
    exposing
        ( Error
        , Gadget(..)
        , IR(..)
        , IRType(..)
        , Variant(..)
        , VariantType(..)
        )
import Result.Extra
import Set


{-| TODO
-}
type alias Gadget a =
    Gadget.IR.Gadget a


{-| TODO
-}
type RecordGadgetBuilder input output
    = RecordGadgetBuilder
        { fromInput : input -> List IR
        , toOutput : List IR -> Result Error output
        , irType : List IRType
        }


{-| TODO
-}
type CustomGadgetBuilder input hasAtLeastOneVariant output
    = CustomGadgetBuilder
        { match : input
        , fromIR : IR -> Result Error output
        , variantTypes : List VariantType
        , index : Int
        }


{-| TODO
-}
bool : Gadget Bool
bool =
    Gadget
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
char : Gadget Char
char =
    Gadget
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
string : Gadget String
string =
    Gadget
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
int : Gadget Int
int =
    Gadget
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
float : Gadget Float
float =
    Gadget
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
list : Gadget a -> Gadget (List a)
list (Gadget item) =
    Gadget
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
    Gadget comparable
    -> Gadget v
    -> Gadget (Dict.Dict comparable v)
dict key value =
    list (tuple key value)
        |> map Dict.fromList Dict.toList


{-| TODO
-}
set :
    Gadget comparable
    -> Gadget (Set.Set comparable)
set value =
    list value
        |> map Set.fromList Set.toList


{-| TODO
-}
array : Gadget a -> Gadget (Array.Array a)
array item =
    list item
        |> map Array.fromList Array.toList


{-| TODO
-}
maybe : Gadget a -> Gadget (Maybe a)
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
result : Gadget x -> Gadget a -> Gadget (Result x a)
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
tuple : Gadget a -> Gadget b -> Gadget ( a, b )
tuple a b =
    record Tuple.pair
        |> field Tuple.first a
        |> field Tuple.second b
        |> endRecord


{-| TODO
-}
triple : Gadget a -> Gadget b -> Gadget c -> Gadget ( a, b, c )
triple a b c =
    record (\a_ b_ c_ -> ( a_, b_, c_ ))
        |> field (\( a_, _, _ ) -> a_) a
        |> field (\( _, b_, _ ) -> b_) b
        |> field (\( _, _, c_ ) -> c_) c
        |> endRecord


{-| TODO
-}
custom : input -> CustomGadgetBuilder input Never output
custom match =
    CustomGadgetBuilder
        { match = match
        , index = 0
        , fromIR = \_ -> Err "custom toOutput failed"
        , variantTypes = []
        }


{-| TODO
-}
variant0 :
    output
    -> CustomGadgetBuilder (IR -> input) variantType output
    -> CustomGadgetBuilder input () output
variant0 ctor (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
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
    -> Gadget arg1
    -> CustomGadgetBuilder ((arg1 -> IR) -> input) variantType output
    -> CustomGadgetBuilder input () output
variant1 ctor (Gadget argfns) (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
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
    -> Gadget arg1
    -> Gadget arg2
    -> CustomGadgetBuilder ((arg1 -> arg2 -> IR) -> input) variantType output
    -> CustomGadgetBuilder input () output
variant2 ctor (Gadget arg1fns) (Gadget arg2fns) (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
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
    -> Gadget arg1
    -> Gadget arg2
    -> Gadget arg3
    -> CustomGadgetBuilder ((arg1 -> arg2 -> arg3 -> IR) -> input) variantType output
    -> CustomGadgetBuilder input () output
variant3 ctor (Gadget arg1fns) (Gadget arg2fns) (Gadget arg3fns) (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
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
    -> Gadget arg1
    -> Gadget arg2
    -> Gadget arg3
    -> Gadget arg4
    -> CustomGadgetBuilder ((arg1 -> arg2 -> arg3 -> arg4 -> IR) -> input) variantType output
    -> CustomGadgetBuilder input () output
variant4 ctor (Gadget arg1fns) (Gadget arg2fns) (Gadget arg3fns) (Gadget arg4fns) (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
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
    -> Gadget arg1
    -> Gadget arg2
    -> Gadget arg3
    -> Gadget arg4
    -> Gadget arg5
    -> CustomGadgetBuilder ((arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> IR) -> input) variantType output
    -> CustomGadgetBuilder input () output
variant5 ctor (Gadget arg1fns) (Gadget arg2fns) (Gadget arg3fns) (Gadget arg4fns) (Gadget arg5fns) (CustomGadgetBuilder prev) =
    CustomGadgetBuilder
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
endCustom : CustomGadgetBuilder (a -> IR) () a -> Gadget a
endCustom (CustomGadgetBuilder prev) =
    Gadget
        { fromInput = prev.match
        , toOutput = prev.fromIR
        , irType =
            case List.reverse prev.variantTypes of
                [] ->
                    -- we know this can't happen, because if the second type
                    -- variable of CustomGadgetBuilder is `()`, then we know
                    -- that we've used at least one `variantX` function, so the
                    -- list of variants can't be empty. So it's ok to use a
                    -- spurious Variant0Type here, because this will never get
                    -- produced.
                    CustomType Variant0Type []

                firstVariantType :: restVariantTypes ->
                    CustomType firstVariantType restVariantTypes
        }


{-| TODO
-}
record : output -> RecordGadgetBuilder input output
record ctor =
    RecordGadgetBuilder
        { fromInput = \_ -> []
        , toOutput = \_ -> Ok ctor
        , irType = []
        }


{-| TODO
-}
field :
    (input -> field)
    -> Gadget field
    -> RecordGadgetBuilder input (field -> output)
    -> RecordGadgetBuilder input output
field getter (Gadget gadget) (RecordGadgetBuilder builder) =
    RecordGadgetBuilder
        { fromInput =
            \input ->
                let
                    thisField =
                        gadget.fromInput (getter input)

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
                            (gadget.toOutput thisField)

                    [] ->
                        Err "expecting a Product field"
        , irType =
            gadget.irType :: builder.irType
        }


{-| TODO
-}
endRecord : RecordGadgetBuilder a a -> Gadget a
endRecord (RecordGadgetBuilder builder) =
    Gadget
        { fromInput =
            \input ->
                Product (List.reverse (builder.fromInput input))
        , toOutput =
            \ir ->
                case ir of
                    Product fields ->
                        builder.toOutput (List.reverse fields)

                    _ ->
                        Err "expecting a Product"
        , irType = ProductType (List.reverse builder.irType)
        }


{-| TODO
-}
map :
    (a -> b)
    -> (b -> a)
    -> Gadget a
    -> Gadget b
map aToB bToA (Gadget prev) =
    Gadget
        { fromInput = bToA >> prev.fromInput
        , toOutput = prev.toOutput >> Result.map aToB
        , irType = prev.irType
        }


{-| TODO
-}
label : String -> Gadget a -> Gadget a
label label_ (Gadget c) =
    Gadget
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
