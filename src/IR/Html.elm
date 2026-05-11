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


primitive : H.Html msg -> String -> String -> H.Html msg
primitive quote label value =
    H.dl []
        [ H.div [ HA.class "primitive", HA.class label ]
            [ H.dt [] [ H.strong [] [ H.text label ] ]
            , H.dd [] [ H.span [] [ quote, H.code [] [ H.text value ], quote ] ]
            ]
        ]


quotedPrimitive : String -> String -> String -> H.Html msg
quotedPrimitive quote =
    primitive (H.span [ HA.class "quote" ] [ H.text quote ])


unquotedPrimitive : String -> String -> H.Html msg
unquotedPrimitive =
    primitive (H.text "")


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
            unquotedPrimitive "Bool"
                (if b then
                    "True"

                 else
                    "False"
                )

        IR.Char c ->
            quotedPrimitive "'" "Char" (String.fromChar c)

        IR.String s ->
            quotedPrimitive "\"" "String" s

        IR.Int i ->
            unquotedPrimitive "Int" (String.fromInt i)

        IR.Float f ->
            unquotedPrimitive "Float" (String.fromFloat f)

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
            in
            combinator
                "Custom"
                ("variant #"
                    ++ String.fromInt selected
                    ++ " with "
                    ++ count args
                    ++ " arguments"
                )
                args

        IR.Product fields ->
            combinator
                "Product"
                ("with " ++ count fields ++ " fields")
                fields

        IR.List items ->
            combinator
                "List"
                ("with " ++ count items ++ " items")
                items


count : List a -> String
count =
    String.fromInt << List.length
