module TestHelpers exposing (..)

import Gadget


type alias Record =
    { bool : Bool
    , int : Int
    , float : Float
    , string : String
    , char : Char
    , custom : Custom
    }


recordCodec : Gadget.Gadget Record
recordCodec =
    Gadget.record Record
        |> Gadget.field .bool Gadget.bool
        |> Gadget.field .int Gadget.int
        |> Gadget.field .float Gadget.float
        |> Gadget.field .string Gadget.string
        |> Gadget.field .char Gadget.char
        |> Gadget.field .custom customCodec
        |> Gadget.endRecord


type Custom
    = Var0
    | Var1 (List Bool)
    | Var2 Int ( Bool, Char )
    | Var3 Bool Bool Bool
    | Var4 Bool Bool Bool Bool
    | Var5 Bool Bool Bool Bool Bool


customCodec : Gadget.Gadget Custom
customCodec =
    Gadget.custom
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
        |> Gadget.variant0 Var0
        |> Gadget.variant1 Var1 (Gadget.list Gadget.bool)
        |> Gadget.variant2 Var2 Gadget.int (Gadget.tuple Gadget.bool Gadget.char)
        |> Gadget.variant3 Var3 Gadget.bool Gadget.bool Gadget.bool
        |> Gadget.variant4 Var4 Gadget.bool Gadget.bool Gadget.bool Gadget.bool
        |> Gadget.variant5 Var5 Gadget.bool Gadget.bool Gadget.bool Gadget.bool Gadget.bool
        |> Gadget.endCustom
