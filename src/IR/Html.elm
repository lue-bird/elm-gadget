module IR.Html exposing (..)

import Html as H
import Html.Attributes as HA
import IR


view : IR.Codec input output -> input -> H.Html msg
view codec value =
    IR.fromInput codec value
        |> htmlAdapter
        |> List.singleton
        |> H.article [ HA.class "elm-ir" ]


primitive : String -> String -> H.Html msg
primitive label value =
    H.dl []
        [ H.div [ HA.class "primitive", HA.class label ]
            [ H.dt [] [ H.text label ]
            , H.dd [] [ H.code [] [ H.text value ] ]
            ]
        ]


combinator : String -> String -> List IR.IR -> H.Html msg
combinator label meta items =
    if List.isEmpty items then
        H.div [ HA.class "combinator", HA.class label ]
            [ H.summary []
                [ H.strong [] [ H.text label ]
                , H.text (" " ++ meta)
                ]
            ]

    else
        H.details [ HA.class "combinator", HA.class label ]
            [ H.summary []
                [ H.strong [] [ H.text label ]
                , H.text (" " ++ meta)
                ]
            , H.ol
                []
                (List.map (\item -> H.li [] [ htmlAdapter item ]) items)
            ]


htmlAdapter : IR.IR -> H.Html msg
htmlAdapter irValue =
    case irValue of
        IR.Bool b ->
            primitive "Bool"
                (if b then
                    "True"

                 else
                    "False"
                )

        IR.Char c ->
            primitive "Char" ("'" ++ String.fromChar c ++ "'")

        IR.String s ->
            primitive "String" ("\"" ++ s ++ "\"")

        IR.Int i ->
            primitive "Int" (String.fromInt i)

        IR.Float f ->
            primitive "Float" (String.fromFloat f)

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
            combinator
                "Custom"
                ("variant #"
                    ++ String.fromInt selected
                    ++ " with "
                    ++ String.fromInt (List.length args)
                    ++ " arguments"
                )
                args

        IR.Product fields ->
            combinator
                "Product"
                ("with " ++ String.fromInt (List.length fields) ++ " fields")
                fields

        IR.List items ->
            combinator
                "List"
                ("with " ++ String.fromInt (List.length items) ++ " items")
                items
