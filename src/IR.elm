module IR exposing
    ( Codec
    , IR(..), Variant(..), fromInput, toOutput, Error
    , IRType(..), VariantType(..), irType
    , bool, char, string, int, float
    , list, array, dict, set, maybe, result, tuple, triple
    , Transformer, record, field
    , CustomCodec, custom
    , variant0, variant1, variant2, variant3, variant4, variant5
    , endCustom
    , map, contramap, andThen
    , label
    )

{-|


# elm-ir

Convert between Elm data types and an intermediate representation (IR)


## IR codecs

@docs Codec


## IR values

@docs IR, Variant, fromInput, toOutput, Error


## IR types

@docs IRType, VariantType, irType


## IR primitives

@docs bool, char, string, int, float


## IR combinators


### Common data types

@docs list, array, dict, set, maybe, result, tuple, triple


### Record types

@docs Transformer, record, field


### Custom types

@docs CustomCodec, custom
@docs variant0, variant1, variant2, variant3, variant4, variant5
@docs endCustom


### Transforming codec input and output

@docs map, contramap, andThen


### Labelling codecs

@docs label

-}

import Array
import Dict
import Result.Extra
import Set


{-| TODO
-}
type alias Error =
    String


{-| TODO
-}
type alias Codec a =
    Transformer a a


{-| TODO
-}
type Transformer input output
    = Codec
        { fromInput : input -> IR
        , toOutput : IR -> Result Error output
        , irType : IRType
        }


{-| TODO
-}
type IR
    = Bool Bool
    | Char Char
    | String String
    | Int Int
    | Float Float
    | Custom Int Variant
    | Product (List IR)
    | List (List IR)
    | Labelled String IR


{-| TODO
-}
type Variant
    = Variant0
    | Variant1 IR
    | Variant2 IR IR
    | Variant3 IR IR IR
    | Variant4 IR IR IR IR
    | Variant5 IR IR IR IR IR


{-| TODO
-}
type IRType
    = BoolType
    | CharType
    | StringType
    | IntType
    | FloatType
    | CustomType VariantType (List VariantType)
    | ProductType (List IRType)
    | ListType IRType
    | LabelledType String IRType


{-| TODO
-}
type VariantType
    = Variant0Type
    | Variant1Type IRType
    | Variant2Type IRType IRType
    | Variant3Type IRType IRType IRType
    | Variant4Type IRType IRType IRType IRType
    | Variant5Type IRType IRType IRType IRType IRType


{-| TODO
-}
fromInput : Codec a -> a -> IR
fromInput (Codec c) input =
    c.fromInput input


{-| TODO
-}
irType : Codec a -> IRType
irType (Codec c) =
    c.irType


{-| TODO
-}
toOutput : Codec a -> IR -> Result Error a
toOutput (Codec c) a =
    c.toOutput a


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
        |> contramap Dict.toList
        |> map Dict.fromList


{-| TODO
-}
set :
    Codec comparable
    -> Codec (Set.Set comparable)
set value =
    list value
        |> contramap Set.toList
        |> map Set.fromList


{-| TODO
-}
array : Codec a -> Codec (Array.Array a)
array item =
    list item
        |> contramap Array.toList
        |> map Array.fromList


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


{-| TODO
-}
triple : Codec a -> Codec b -> Codec c -> Codec ( a, b, c )
triple a b c =
    record (\a_ b_ c_ -> ( a_, b_, c_ ))
        |> field (\( a_, _, _ ) -> a_) a
        |> field (\( _, b_, _ ) -> b_) b
        |> field (\( _, _, c_ ) -> c_) c


{-| TODO
-}
type CustomCodec input hasAtLeastOneVariant output
    = CustomCodec
        { match : input
        , fromIR : IR -> Result Error output
        , variantTypes : List VariantType
        , index : Int
        }


{-| TODO
-}
custom : input -> CustomCodec input Never output
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
    -> CustomCodec (IR -> input) variantType output
    -> CustomCodec input () output
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
    -> CustomCodec ((arg1 -> IR) -> input) variantType output
    -> CustomCodec input () output
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
    -> CustomCodec ((arg1 -> arg2 -> IR) -> input) variantType output
    -> CustomCodec input () output
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
    -> CustomCodec ((arg1 -> arg2 -> arg3 -> IR) -> input) variantType output
    -> CustomCodec input () output
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
    -> CustomCodec ((arg1 -> arg2 -> arg3 -> arg4 -> IR) -> input) variantType output
    -> CustomCodec input () output
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
    -> CustomCodec ((arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> IR) -> input) variantType output
    -> CustomCodec input () output
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
endCustom : CustomCodec (a -> IR) () a -> Codec a
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
record : output -> Transformer input output
record ctor =
    Codec
        { fromInput = \_ -> Product []
        , toOutput =
            \ir ->
                case ir of
                    Product [] ->
                        Ok ctor

                    _ ->
                        Err "succeed toOutput failed"
        , irType = ProductType []
        }


{-| TODO
-}
field :
    (input -> field)
    -> Codec field
    -> Transformer input (field -> output)
    -> Transformer input output
field getter (Codec this) (Codec prev) =
    Codec
        { fromInput =
            \a ->
                case prev.fromInput a of
                    Product prevFields ->
                        Product (this.fromInput (getter a) :: prevFields)

                    _ ->
                        Product [ this.fromInput (getter a) ]
        , toOutput =
            \ir ->
                case ir of
                    Product (thisField :: prevFields) ->
                        Result.map2 (\ctor val -> ctor val)
                            (prev.toOutput (Product prevFields))
                            (this.toOutput thisField)

                    _ ->
                        Err "andMap toOutput failed"
        , irType =
            case prev.irType of
                ProductType prevFieldTypes ->
                    ProductType (this.irType :: prevFieldTypes)

                _ ->
                    ProductType [ this.irType ]
        }


{-| TODO
-}
map :
    (a -> b)
    -> Transformer input a
    -> Transformer input b
map f (Codec prev) =
    Codec
        { fromInput = prev.fromInput
        , toOutput = prev.toOutput >> Result.map f
        , irType = prev.irType
        }


{-| TODO
-}
contramap :
    (b -> a)
    -> Transformer a output
    -> Transformer b output
contramap f (Codec prev) =
    Codec
        { fromInput = f >> prev.fromInput
        , toOutput = prev.toOutput
        , irType = prev.irType
        }


{-| TODO
-}
andThen :
    (a -> Result Error b)
    -> Transformer input a
    -> Transformer input b
andThen f (Codec prev) =
    Codec
        { fromInput = prev.fromInput
        , toOutput = prev.toOutput >> Result.andThen f
        , irType = prev.irType
        }


{-| TODO
-}
label : String -> Codec a -> Codec a
label label_ (Codec c) =
    Codec
        { fromInput = c.fromInput >> Labelled label_
        , toOutput =
            \ir ->
                case ir of
                    Labelled _ innerIR ->
                        c.toOutput innerIR

                    other ->
                        Err ("override toOutput failed " ++ Debug.toString other)
        , irType = LabelledType label_ c.irType
        }
