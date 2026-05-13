module IR.Json exposing (..)

import IR
import Json.Decode as JD
import Json.Encode as JE


encode : IR.Codec input output -> input -> JE.Value
encode codec value =
    value
        |> IR.fromInput codec
        |> encodeAdapter


decoder : IR.Codec input output -> JD.Decoder output
decoder codec =
    decodeAdapter
        |> JD.andThen
            (\irValue ->
                case IR.toOutput codec irValue of
                    Ok s ->
                        JD.succeed s

                    Err e ->
                        JD.fail e
            )


encodeAdapter : IR.IR -> JE.Value
encodeAdapter irValue =
    case irValue of
        IR.Labelled label innerValue ->
            JE.object
                [ ( "override"
                  , JE.object
                        [ ( "label", JE.string label )
                        , ( "value", encodeAdapter innerValue )
                        ]
                  )
                ]

        IR.Bool b ->
            JE.object
                [ ( "bool", JE.bool b ) ]

        IR.Char c ->
            JE.object
                [ ( "char", JE.string (String.fromChar c) ) ]

        IR.String s ->
            JE.object
                [ ( "string", JE.string s ) ]

        IR.Int i ->
            JE.object
                [ ( "int", JE.int i ) ]

        IR.Float f ->
            JE.object
                [ ( "float", JE.float f ) ]

        IR.Custom selected variant ->
            JE.object
                [ ( "custom"
                  , JE.object
                        [ ( "tag", JE.int selected )
                        , ( "args"
                          , JE.list encodeAdapter
                                (case variant of
                                    IR.Variant0 ->
                                        []

                                    IR.Variant1 arg ->
                                        [ arg ]

                                    IR.Variant2 arg1 arg2 ->
                                        [ arg1
                                        , arg2
                                        ]

                                    IR.Variant3 arg1 arg2 arg3 ->
                                        [ arg1
                                        , arg2
                                        , arg3
                                        ]

                                    IR.Variant4 arg1 arg2 arg3 arg4 ->
                                        [ arg1
                                        , arg2
                                        , arg3
                                        , arg4
                                        ]

                                    IR.Variant5 arg1 arg2 arg3 arg4 arg5 ->
                                        [ arg1
                                        , arg2
                                        , arg3
                                        , arg4
                                        , arg5
                                        ]
                                )
                          )
                        ]
                  )
                ]

        IR.Product fields ->
            JE.object
                [ ( "product"
                  , JE.list encodeAdapter fields
                  )
                ]

        IR.List items ->
            JE.object
                [ ( "list"
                  , JE.list encodeAdapter items
                  )
                ]


decodeAdapter : JD.Decoder IR.IR
decodeAdapter =
    JD.oneOf
        [ JD.field "override"
            (JD.map2 (\label value -> IR.Labelled label value)
                (JD.field "label" JD.string)
                (JD.field "value" (JD.lazy (\() -> decodeAdapter)))
            )
        , JD.field "bool" JD.bool |> JD.map IR.Bool
        , JD.field "char" JD.string
            |> JD.andThen
                (\s ->
                    case String.uncons s of
                        Nothing ->
                            JD.fail "not a char"

                        Just ( c, _ ) ->
                            JD.succeed (IR.Char c)
                )
        , JD.field "string" JD.string |> JD.map IR.String
        , JD.field "int" JD.int |> JD.map IR.Int
        , JD.field "float" JD.float |> JD.map IR.Float
        , JD.field "custom"
            (JD.map2
                (\selected args ->
                    Maybe.map (IR.Custom selected) <|
                        case args of
                            [] ->
                                Just IR.Variant0

                            [ arg ] ->
                                Just (IR.Variant1 arg)

                            [ arg1, arg2 ] ->
                                Just (IR.Variant2 arg1 arg2)

                            [ arg1, arg2, arg3 ] ->
                                Just (IR.Variant3 arg1 arg2 arg3)

                            [ arg1, arg2, arg3, arg4 ] ->
                                Just (IR.Variant4 arg1 arg2 arg3 arg4)

                            [ arg1, arg2, arg3, arg4, arg5 ] ->
                                Just (IR.Variant5 arg1 arg2 arg3 arg4 arg5)

                            _ ->
                                Nothing
                )
                (JD.field "tag" JD.int)
                (JD.field "args" (JD.list (JD.lazy (\() -> decodeAdapter))))
                |> JD.andThen
                    (\maybeIR ->
                        case maybeIR of
                            Nothing ->
                                JD.fail ""

                            Just ir ->
                                JD.succeed ir
                    )
            )
        , JD.field "product"
            (JD.list (JD.lazy (\() -> decodeAdapter))
                |> JD.map IR.Product
            )
        , JD.field "list"
            (JD.list (JD.lazy (\() -> decodeAdapter))
                |> JD.map IR.List
            )
        ]
