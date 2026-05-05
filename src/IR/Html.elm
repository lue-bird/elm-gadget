module IR.Html exposing (..)

import Html as H
import IR


view : IR.Codec input output -> input -> H.Html msg
view codec value =
    let
        (IR.IR irValue) =
            IR.fromInput codec value
    in
    htmlAdapter irValue


kv k v =
    H.dl [] [ H.div [] [ H.dt [] [ H.text k ], H.dd [] [ H.text v ] ] ]


htmlAdapter : IR.IRValue -> H.Html msg
htmlAdapter irValue =
    case irValue of
        IR.Bool b ->
            kv "Bool"
                (if b then
                    "True"

                 else
                    "False"
                )

        IR.Char c ->
            kv "Char" (String.fromChar c)

        IR.String s ->
            kv "String" s

        IR.Int i ->
            kv "Int" (String.fromInt i)

        IR.Float f ->
            kv "Float" (String.fromFloat f)

        IR.Custom selected variant ->
            let
                args =
                    case variant of
                        IR.Variant0 ->
                            []

                        IR.Variant1 arg ->
                            [ arg ]

                        IR.Variant2 arg1 arg2 ->
                            [ arg1
                            , arg2
                            ]
            in
            H.details []
                [ H.summary [] [ H.text ("Custom type, variant " ++ String.fromInt selected ++ " with " ++ String.fromInt (List.length args) ++ " arguments") ]
                , H.ol [] (List.map (\arg -> H.li [] [ htmlAdapter arg ]) args)
                ]

        IR.Product fields ->
            H.details []
                [ H.summary [] [ H.text ("Product type with " ++ String.fromInt (List.length fields) ++ " fields") ]
                , H.ol [] (List.map (\field -> H.li [] [ htmlAdapter field ]) fields)
                ]

        IR.List items ->
            H.details []
                [ H.summary [] [ H.text ("List type with " ++ String.fromInt (List.length items) ++ " items") ]
                , H.ol [] (List.map (\item -> H.li [] [ htmlAdapter item ]) items)
                ]
