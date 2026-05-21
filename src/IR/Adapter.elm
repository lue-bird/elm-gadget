module IR.Adapter exposing
    ( Codec
    , Error
    , IR(..)
    , IRType(..)
    , Transformer(..)
    , Variant(..)
    , VariantType(..)
    , fromInput
    , irType
    , toOutput
    )

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
