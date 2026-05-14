module TestHelpers exposing (..)

import IR


type alias Record =
    { bool : Bool
    , int : Int
    , float : Float
    , string : String
    , char : Char
    , custom : Custom
    }


recordCodec : IR.Codec Record
recordCodec =
    IR.record Record
        |> IR.field .bool IR.bool
        |> IR.field .int IR.int
        |> IR.field .float IR.float
        |> IR.field .string IR.string
        |> IR.field .char IR.char
        |> IR.field .custom customCodec
        |> IR.endRecord


type Custom
    = Var0
    | Var1 (List Bool)
    | Var2 Int ( Bool, Char )
    | Var3 Bool Bool Bool
    | Var4 Bool Bool Bool Bool
    | Var5 Bool Bool Bool Bool Bool


customCodec : IR.Codec Custom
customCodec =
    IR.custom
        (\v0 v1 v2 v3 v4 v5 v ->
            case v of
                Var0 ->
                    v0

                Var1 l ->
                    v1 l

                Var2 i r ->
                    v2 i r

                Var3 b1 b2 b3 ->
                    v3 b1 b2 b3

                Var4 b1 b2 b3 b4 ->
                    v4 b1 b2 b3 b4

                Var5 b1 b2 b3 b4 b5 ->
                    v5 b1 b2 b3 b4 b5
        )
        |> IR.variant0 Var0
        |> IR.variant1 Var1 (IR.list IR.bool)
        |> IR.variant2 Var2 IR.int (IR.tuple IR.bool IR.char)
        |> IR.variant3 Var3 IR.bool IR.bool IR.bool
        |> IR.variant4 Var4 IR.bool IR.bool IR.bool IR.bool
        |> IR.variant5 Var5 IR.bool IR.bool IR.bool IR.bool IR.bool
        |> IR.endCustom
