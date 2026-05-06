module IR.Html exposing (..)

import Html as H
import IR


view : IR.Codec input output -> input -> H.Html msg
view codec value =
    IR.fromInput codec value
        |> IR.run htmlAdapter


keyValuePair : String -> String -> H.Html msg
keyValuePair k v =
    H.dl []
        [ H.div []
            [ H.dt [] [ H.text k ]
            , H.dd [] [ H.text v ]
            ]
        ]


expandable : String -> List IR.IRValue -> H.Html msg
expandable label items =
    H.details []
        [ H.summary [] [ H.text label ]
        , H.ol [] (List.map (\item -> H.li [] [ htmlAdapter item ]) items)
        ]


htmlAdapter : IR.IRValue -> H.Html msg
htmlAdapter irValue =
    case irValue of
        IR.Bool b ->
            keyValuePair "Bool"
                (if b then
                    "True"

                 else
                    "False"
                )

        IR.Char c ->
            keyValuePair "Char" (String.fromChar c)

        IR.String s ->
            keyValuePair "String" s

        IR.Int i ->
            keyValuePair "Int" (String.fromInt i)

        IR.Float f ->
            keyValuePair "Float" (String.fromFloat f)

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
            expandable
                ("Custom type, variant " ++ String.fromInt selected ++ " with " ++ String.fromInt (List.length args) ++ " arguments")
                args

        IR.Product fields ->
            expandable
                ("Product type with " ++ String.fromInt (List.length fields) ++ " fields")
                fields

        IR.List items ->
            expandable
                ("List type with " ++ String.fromInt (List.length items) ++ " items")
                items
