module IR.String exposing (parser, print)

import IR
import Parser as P exposing ((|.), (|=), Parser)


print : IR.Codec input output -> input -> String
print codec value =
    IR.fromInput codec value
        |> printAdapter


primitive : String -> String -> String
primitive label value =
    label ++ "(" ++ value ++ ")"


combinator : String -> String -> List IR.IR -> String
combinator label meta items =
    label
        ++ meta
        ++ "["
        ++ String.join "," (List.map printAdapter items)
        ++ "]"


printAdapter : IR.IR -> String
printAdapter irValue =
    case irValue of
        IR.Bool b ->
            primitive "b"
                (if b then
                    "1"

                 else
                    "0"
                )

        IR.Char c ->
            "c" ++ quoteString (String.fromChar c)

        IR.String s ->
            "s" ++ quoteString s

        IR.Int i ->
            primitive "i" (String.fromInt i)

        IR.Float f ->
            primitive "f" (String.fromFloat f)

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
                "u"
                (String.fromInt selected)
                args

        IR.Product fields ->
            combinator
                "p"
                ""
                fields

        IR.List items ->
            combinator
                "l"
                ""
                items


parser : IR.Codec input output -> Parser output
parser codec =
    irParser
        |> P.andThen
            (\ir ->
                case IR.toOutput codec ir of
                    Ok output ->
                        P.succeed output

                    Err _ ->
                        P.problem "failed to convert IR"
            )


irParser : Parser IR.IR
irParser =
    P.oneOf
        [ primitiveParser "b" IR.Bool boolParser
        , primitiveParser "i" IR.Int intParser
        , primitiveParser "f" IR.Float floatParser
        , charParser
        , stringParser
        , listParser
        , productParser
        , customParser
        ]


listParser : Parser IR.IR
listParser =
    P.sequence
        { start = "l["
        , item = P.lazy (\() -> irParser)
        , end = "]"
        , separator = ","
        , spaces = P.spaces
        , trailing = P.Forbidden
        }
        |> P.map IR.List


productParser : Parser IR.IR
productParser =
    P.sequence
        { start = "p["
        , item = P.lazy (\() -> irParser)
        , end = "]"
        , separator = ","
        , spaces = P.spaces
        , trailing = P.Forbidden
        }
        |> P.map IR.Product


customParser : Parser IR.IR
customParser =
    P.succeed IR.Custom
        |. P.token "u"
        |= P.int
        |= (P.sequence
                { start = "["
                , item = P.lazy (\() -> irParser)
                , end = "]"
                , separator = ","
                , spaces = P.spaces
                , trailing = P.Forbidden
                }
                |> P.andThen
                    (\args ->
                        case args of
                            [] ->
                                P.succeed IR.Variant0

                            [ arg ] ->
                                P.succeed (IR.Variant1 arg)

                            [ arg1, arg2 ] ->
                                P.succeed (IR.Variant2 arg1 arg2)

                            _ ->
                                P.problem "variant has too many args"
                    )
           )


primitiveParser : String -> (keep -> IR.IR) -> Parser keep -> Parser IR.IR
primitiveParser marker ctor innerParser =
    P.succeed ctor
        |. P.token (marker ++ "(")
        |= innerParser
        |. P.token ")"


boolParser : Parser Bool
boolParser =
    P.int
        |> P.andThen
            (\int ->
                case int of
                    0 ->
                        P.succeed False

                    1 ->
                        P.succeed True

                    _ ->
                        P.problem "Not a bool"
            )


charParser : Parser IR.IR
charParser =
    (P.succeed identity
        |. P.token "c("
        |= P.loop "" stringParserHelp
    )
        |> P.andThen
            (\str ->
                case String.uncons str of
                    Nothing ->
                        P.problem "Not a char"

                    Just ( c, _ ) ->
                        P.succeed (IR.Char c)
            )


intParser : Parser Int
intParser =
    P.oneOf
        [ P.succeed negate
            |. P.symbol "-"
            |= P.int
        , P.int
        ]


{-| In our case, we can't use `P.float` from elm/parser because it has a
bug with very large numbers - see <https://github.com/elm/parser/issues/58>.

This implementation is probably slower and won't handle things like

    > 1.79*10.0^308
    1.79e+308 : Float
    > String.fromFloat(1.79*10.0^308)
    "1.79e+308" : String

But that's ok in our case, because we don't need to handle Float literals, only
values produced by `String.fromFloat`.

-}
floatParser : Parser Float
floatParser =
    P.oneOf
        [ P.token "Infinity" |> P.map (\_ -> 1 / 0)
        , P.token "-Infinity" |> P.map (\_ -> -1 / 0)
        , P.token "NaN" |> P.map (\_ -> 0 / 0)
        , P.succeed negate
            |. P.symbol "-"
            |= floatParserHelp
        , floatParserHelp
        ]


floatParserHelp : Parser Float
floatParserHelp =
    let
        oneOrMoreDigits =
            P.succeed ()
                |. P.chompIf Char.isDigit
                |. P.chompWhile Char.isDigit
    in
    P.succeed
        (\start { usesENotation } end source ->
            { chompedString = String.slice start end source
            , usesENotation = usesENotation
            }
        )
        |= P.getOffset
        |. oneOrMoreDigits
        |= P.oneOf
            [ P.succeed identity
                |. P.chompIf (\c -> c == '.')
                |. oneOrMoreDigits
                |= P.oneOf
                    [ -- it's a number like `1.01e+21`
                      P.succeed { usesENotation = True }
                        |. P.chompIf (\c -> c == 'e')
                        |. P.oneOf
                            [ P.chompIf (\c -> c == '+')
                            , P.chompIf (\c -> c == '-')
                            ]
                        |. oneOrMoreDigits

                    -- it's a number like `1.1`
                    , P.succeed { usesENotation = False }
                    ]

            -- it's a number like `1`
            , P.succeed { usesENotation = False }
            ]
        |= P.getOffset
        |= P.getSource
        |> P.andThen
            (\{ chompedString, usesENotation } ->
                if usesENotation then
                    -- bail out and use `P.float` instead
                    case P.run P.float chompedString of
                        Ok f ->
                            P.succeed f

                        Err _ ->
                            P.problem "Not a float"

                else
                    -- just use `String.toFloat`
                    case String.toFloat chompedString of
                        Just f ->
                            P.succeed f

                        Nothing ->
                            P.problem "Not a Float"
            )


{-| This function comes from
<https://github.com/myrho/elm-parser-extras/tree/1.0.1> but the implementation
there seems to have a bug with an unguarded `chompWhile` leading to infinite
looping. Fixed version is here.
-}
stringParser : Parser IR.IR
stringParser =
    P.succeed IR.String
        |. P.token "s("
        |= P.loop "" stringParserHelp


stringParserHelp : String -> Parser (P.Step String String)
stringParserHelp string =
    P.oneOf
        [ P.token (String.fromList [ '/', ')' ])
            |> P.map (\_ -> string ++ String.fromChar ')' |> P.Loop)
        , P.token (String.fromList [ '/', '/' ])
            |> P.map (\_ -> string ++ String.fromChar '/' |> P.Loop)
        , P.chompIf ((==) ')')
            |> P.map (\_ -> P.Done string)
        , P.chompIf ((==) '/')
            |> P.map (\_ -> string ++ String.fromChar '/' |> P.Loop)
        , P.succeed ()
            |. P.chompIf (\c -> c /= ')' && c /= '/')
            |. P.chompWhile (\c -> c /= ')' && c /= '/')
            |> P.getChompedString
            |> P.map (\s -> string ++ s |> P.Loop)
        ]


quoteString : String -> String
quoteString str =
    let
        quoted =
            str
                |> String.replace "/" "//"
                |> String.replace ")" "/)"
    in
    "(" ++ quoted ++ ")"
