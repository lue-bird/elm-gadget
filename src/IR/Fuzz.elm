module IR.Fuzz exposing (..)

import Fuzz
import IR


fuzzer : IR.Codec input output -> Fuzz.Fuzzer output
fuzzer codec =
    IR.irType codec
        |> fuzzAdapter
        |> Fuzz.andThen
            (\x ->
                case IR.toOutput codec x of
                    Ok y ->
                        Fuzz.constant y

                    Err IR.Error ->
                        Fuzz.invalid "invalid fuzzer!"
            )


fuzzAdapter : IR.IRType -> Fuzz.Fuzzer IR.IR
fuzzAdapter irType =
    case irType of
        IR.BoolType ->
            Fuzz.bool |> Fuzz.map IR.Bool

        IR.CharType ->
            Fuzz.char |> Fuzz.map IR.Char

        IR.StringType ->
            Fuzz.string |> Fuzz.map IR.String

        IR.IntType ->
            Fuzz.int |> Fuzz.map IR.Int

        IR.FloatType ->
            Fuzz.niceFloat |> Fuzz.map IR.Float

        IR.CustomType firstVariant restVariants ->
            Fuzz.oneOf
                (List.indexedMap
                    (\idx variant ->
                        case variant of
                            IR.Variant0Type ->
                                Fuzz.constant
                                    (IR.Custom idx IR.Variant0)

                            IR.Variant1Type arg ->
                                Fuzz.map
                                    (\a -> IR.Custom idx (IR.Variant1 a))
                                    (fuzzAdapter arg)

                            IR.Variant2Type arg1 arg2 ->
                                Fuzz.map2
                                    (\a1 a2 -> IR.Custom idx (IR.Variant2 a1 a2))
                                    (fuzzAdapter arg1)
                                    (fuzzAdapter arg2)

                            IR.Variant3Type arg1 arg2 arg3 ->
                                Fuzz.map3
                                    (\a1 a2 a3 -> IR.Custom idx (IR.Variant3 a1 a2 a3))
                                    (fuzzAdapter arg1)
                                    (fuzzAdapter arg2)
                                    (fuzzAdapter arg3)

                            IR.Variant4Type arg1 arg2 arg3 arg4 ->
                                Fuzz.map4
                                    (\a1 a2 a3 a4 -> IR.Custom idx (IR.Variant4 a1 a2 a3 a4))
                                    (fuzzAdapter arg1)
                                    (fuzzAdapter arg2)
                                    (fuzzAdapter arg3)
                                    (fuzzAdapter arg4)

                            IR.Variant5Type arg1 arg2 arg3 arg4 arg5 ->
                                Fuzz.map5
                                    (\a1 a2 a3 a4 a5 -> IR.Custom idx (IR.Variant5 a1 a2 a3 a4 a5))
                                    (fuzzAdapter arg1)
                                    (fuzzAdapter arg2)
                                    (fuzzAdapter arg3)
                                    (fuzzAdapter arg4)
                                    (fuzzAdapter arg5)
                    )
                    (firstVariant :: restVariants)
                )

        IR.ProductType fields ->
            fields
                |> Fuzz.traverse fuzzAdapter
                |> Fuzz.map IR.Product

        IR.ListType itemType ->
            Fuzz.list (fuzzAdapter itemType)
                |> Fuzz.map IR.List
