module IR.Fuzz exposing (..)

import Dict
import Fuzz
import IR.Advanced as IR


fuzzer : IR.Codec input output -> Fuzz.Fuzzer output
fuzzer codec =
    fuzzerWithOverrides [] codec


fuzzerWithOverrides : List ( String, Fuzz.Fuzzer IR.IR ) -> IR.Codec input output -> Fuzz.Fuzzer output
fuzzerWithOverrides overrides codec =
    IR.irType codec
        |> fuzzAdapter (Dict.fromList overrides)
        |> Fuzz.andThen
            (\fuzzedIR ->
                case IR.toOutput codec fuzzedIR of
                    Ok fuzzedOutput ->
                        Fuzz.constant fuzzedOutput

                    Err e ->
                        Fuzz.invalid e
            )


override : String -> IR.Codec input output -> Fuzz.Fuzzer input -> ( String, Fuzz.Fuzzer IR.IR )
override label codec inputFuzzer =
    ( label, Fuzz.map (IR.fromInput codec) inputFuzzer )


fuzzAdapter : Dict.Dict String (Fuzz.Fuzzer IR.IR) -> IR.IRType -> Fuzz.Fuzzer IR.IR
fuzzAdapter overrides irType =
    case irType of
        IR.OverrideType label innerType ->
            Fuzz.map (IR.Override label)
                (case Dict.get label overrides of
                    Just override_ ->
                        override_

                    Nothing ->
                        fuzzAdapter overrides innerType
                )

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
                                    (fuzzAdapter overrides arg)

                            IR.Variant2Type arg1 arg2 ->
                                Fuzz.map2
                                    (\a1 a2 -> IR.Custom idx (IR.Variant2 a1 a2))
                                    (fuzzAdapter overrides arg1)
                                    (fuzzAdapter overrides arg2)

                            IR.Variant3Type arg1 arg2 arg3 ->
                                Fuzz.map3
                                    (\a1 a2 a3 -> IR.Custom idx (IR.Variant3 a1 a2 a3))
                                    (fuzzAdapter overrides arg1)
                                    (fuzzAdapter overrides arg2)
                                    (fuzzAdapter overrides arg3)

                            IR.Variant4Type arg1 arg2 arg3 arg4 ->
                                Fuzz.map4
                                    (\a1 a2 a3 a4 -> IR.Custom idx (IR.Variant4 a1 a2 a3 a4))
                                    (fuzzAdapter overrides arg1)
                                    (fuzzAdapter overrides arg2)
                                    (fuzzAdapter overrides arg3)
                                    (fuzzAdapter overrides arg4)

                            IR.Variant5Type arg1 arg2 arg3 arg4 arg5 ->
                                Fuzz.map5
                                    (\a1 a2 a3 a4 a5 -> IR.Custom idx (IR.Variant5 a1 a2 a3 a4 a5))
                                    (fuzzAdapter overrides arg1)
                                    (fuzzAdapter overrides arg2)
                                    (fuzzAdapter overrides arg3)
                                    (fuzzAdapter overrides arg4)
                                    (fuzzAdapter overrides arg5)
                    )
                    (firstVariant :: restVariants)
                )

        IR.ProductType fields ->
            fields
                |> Fuzz.traverse (fuzzAdapter overrides)
                |> Fuzz.map IR.Product

        IR.ListType itemType ->
            Fuzz.list (fuzzAdapter overrides itemType)
                |> Fuzz.map IR.List
