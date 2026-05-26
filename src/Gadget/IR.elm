module Gadget.IR exposing
    ( Gadget(..), Error, IR(..), IRType(..), Variant(..), VariantType(..)
    , fromInput, toOutput, irType
    )

{-|

@docs Gadget, Error, IR, IRType, Variant, VariantType
@docs fromInput, toOutput, irType

-}

import Set


{-| TODO
-}
type alias Error =
    String


{-| TODO
-}
type Gadget a
    = Gadget
        { fromInput : a -> IR
        , toOutput : IR -> Result Error a
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
    | Labelled (Set.Set String) IR


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
    | LabelledType (Set.Set String) IRType


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
fromInput : Gadget a -> a -> IR
fromInput (Gadget c) input =
    c.fromInput input


{-| TODO
-}
irType : Gadget a -> IRType
irType (Gadget c) =
    c.irType


{-| TODO
-}
toOutput : Gadget a -> IR -> Result Error a
toOutput (Gadget c) a =
    c.toOutput a
