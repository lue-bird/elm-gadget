module Main exposing (..)

import Fuzz
import Html
import IR
import IR.Diff
import IR.Fuzz
import IR.Html
import IR.Json
import IR.Random
import IR.String
import Json.Decode as JD
import Json.Encode as JE
import Parser
import Random


type Example
    = Yellow
    | Green String Record
    | Red Char (List Bool)


type alias Record =
    { name : String
    , number : Int
    }


recordCodec : IR.Codec Record Record
recordCodec =
    IR.succeed Record
        |> IR.andMap .name (IR.string |> IR.label "name")
        |> IR.andMap .number (IR.int |> IR.label "number")


exampleCodec : IR.Codec Example Example
exampleCodec =
    IR.custom
        (\red yellow green value ->
            case value of
                Red c l ->
                    red c l

                Yellow ->
                    yellow

                Green s r ->
                    green s r
        )
        |> IR.variant2 Red IR.char (IR.list IR.bool)
        |> IR.variant0 Yellow
        |> IR.variant2 Green IR.string recordCodec
        |> IR.endCustom


main : Html.Html msg
main =
    let
        codec =
            exampleCodec

        fuzzOverrides =
            [ IR.Fuzz.override "name" IR.string (Fuzz.oneOf (List.map Fuzz.constant [ "Ed", "Mario", "Leonardo", "Jeroen" ]))
            , IR.Fuzz.override "number" IR.int (Fuzz.constant 500)
            ]

        fuzzer =
            IR.Fuzz.fuzzerWithOverrides fuzzOverrides codec

        fuzzed =
            Fuzz.examples 1 fuzzer

        randomOverrides =
            [ IR.Random.override "name" IR.string (Random.uniform "Ed" [ "Mario", "Leonardo", "Jeroen" ])
            , IR.Random.override "number" IR.int (Random.constant 1000)
            ]

        randomGenerator =
            IR.Random.generatorWithOverrides randomOverrides codec

        firstValue =
            Random.step randomGenerator (Random.initialSeed 0)
                |> Tuple.first

        secondValue =
            Random.step randomGenerator (Random.initialSeed 4)
                |> Tuple.first

        diff =
            IR.Diff.diff codec firstValue secondValue

        patched =
            IR.Diff.patch codec diff firstValue

        encoded =
            JE.encode 2 (IR.Json.encode codec firstValue)

        decoded =
            JD.decodeString (IR.Json.decoder codec) encoded

        printed =
            IR.String.print codec firstValue

        parsed =
            Parser.run (IR.String.parser codec) printed
    in
    Html.div []
        [ Html.h1 [] [ Html.text "elm-ir examples" ]
        , head "IR type"
        , show (IR.irType codec)
        , head "Random generator (first value)"
        , show firstValue
        , head "Random generator (second value)"
        , show secondValue
        , head "Diff between first & second values"
        , show diff
        , head "Patch first value with diff"
        , show patched
        , head "Patched value equals second value?"
        , show (patched == Ok secondValue)
        , head "Html viewer (first value)"
        , IR.Html.view codec firstValue
        , head "Html viewer (second value)"
        , IR.Html.view codec secondValue
        , head "Printer (first value)"
        , Html.pre [] [ Html.text printed ]
        , head "Parser (first value)"
        , show parsed
        , head "JSON encoder (first value)"
        , Html.pre [] [ Html.text encoded ]
        , head "JSON decoder (first value)"
        , show decoded
        , head "Fuzzer"
        , show fuzzed
        ]


head : String -> Html.Html msg
head txt =
    Html.h3 [] [ Html.text txt ]


show : a -> Html.Html msg
show a =
    Html.pre [] [ Html.text (Debug.toString a) ]
