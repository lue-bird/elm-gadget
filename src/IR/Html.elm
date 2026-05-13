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


primitive : H.Html msg -> (String -> H.Html msg) -> String -> String -> H.Html msg
primitive quoteHtml valueWrapper typeName value =
    H.dl []
        [ H.div [ HA.class "primitive", HA.class typeName ]
            [ H.dt [] [ H.em [] [ H.text typeName ] ]
            , H.dd [] [ H.span [] [ quoteHtml, valueWrapper value, quoteHtml ] ]
            ]
        ]


labelled : String -> H.Html msg -> H.Html msg
labelled label inner =
    H.dl []
        [ H.div [ HA.class "labelled" ]
            [ H.dt []
                [ H.em [] [ H.text "Label" ]
                , H.code [] [ H.text label ]
                ]
            , H.dd [] [ inner ]
            ]
        ]


quotedPrimitive : String -> String -> String -> H.Html msg
quotedPrimitive quote =
    primitive (H.span [ HA.class "quote" ] [ H.text quote ]) (\value -> H.code [] [ H.text value ])


unquotedPrimitive : String -> String -> H.Html msg
unquotedPrimitive =
    primitive (H.text "") (\value -> H.code [] [ H.text value ])


combinator : String -> String -> List IR.IR -> H.Html msg
combinator typeName meta items =
    if List.isEmpty items then
        H.div [ HA.class "combinator", HA.class typeName ]
            [ H.summary []
                [ H.strong [] [ H.text typeName ]
                , H.text (" " ++ meta)
                ]
            ]

    else
        H.details [ HA.class "combinator", HA.class typeName ]
            [ H.summary []
                [ H.strong [] [ H.text typeName ]
                , H.text (" " ++ meta)
                ]
            , H.ol
                []
                (List.map (\item -> H.li [] [ htmlAdapter item ]) items)
            ]


htmlAdapter : IR.IR -> H.Html msg
htmlAdapter irValue =
    case irValue of
        IR.Labelled label inner ->
            labelled label (htmlAdapter inner)

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
