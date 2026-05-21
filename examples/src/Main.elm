module Main exposing (..)

import Browser
import Fuzz
import Html
import Html.Attributes
import Html.Events
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


type alias Person =
    { name : String
    , heightInCentimetres : Float
    , pets : List Pet
    }


type Pet
    = Dog { name : String }
    | Robot Char Int


personCodec : IR.Codec Person
personCodec =
    IR.record Person
        |> IR.field .name
            (IR.string |> IR.label "name")
        |> IR.field .heightInCentimetres
            (IR.float |> IR.label "heightInCentimetres")
        |> IR.field .pets
            (IR.list petCodec)


petCodec : IR.Codec Pet
petCodec =
    IR.custom
        (\dog robot variant ->
            case variant of
                Dog rec ->
                    dog rec

                Robot series model ->
                    robot series model
        )
        |> IR.variant1 Dog
            (IR.record (\name -> { name = name })
                |> IR.field .name
                    (IR.string |> IR.label "dogName")
            )
        |> IR.variant2 Robot
            (IR.char |> IR.label "series")
            (IR.int |> IR.label "model")
        |> IR.endCustom


main : Program () ( Int, Int ) Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = always Sub.none
        }


type Msg
    = Clicked
    | NewSeeds ( Int, Int )


update msg model =
    case msg of
        Clicked ->
            ( model
            , Random.generate NewSeeds (Random.pair (Random.int 0 Random.maxInt) (Random.int 0 Random.maxInt))
            )

        NewSeeds newSeeds ->
            ( newSeeds
            , Cmd.none
            )


init _ =
    ( ( 0, 1 )
    , Cmd.none
    )


view : ( Int, Int ) -> Html.Html Msg
view ( seed1, seed2 ) =
    let
        codec =
            personCodec

        fuzzOverrides =
            [ IR.Fuzz.override "name" IR.string (Fuzz.oneOf (List.map Fuzz.constant [ "Ed", "Mario", "Leonardo", "Jeroen" ]))
            , IR.Fuzz.override "heightInCentimetres" IR.float (Fuzz.floatRange 160 196)
            , IR.Fuzz.override "dogName" IR.string (Fuzz.oneOf (List.map Fuzz.constant [ "Fido", "Kevin", "Rover", "Fifi" ]))
            , IR.Fuzz.override "series" IR.char (Fuzz.oneOf (List.range 65 90 |> List.map Char.fromCode |> List.map Fuzz.constant))
            , IR.Fuzz.override "model" IR.int (Fuzz.oneOf (List.range 1 5 |> List.map (\n -> n * 1000) |> List.map Fuzz.constant))
            ]

        fuzzer =
            IR.Fuzz.fuzzerWithOverrides fuzzOverrides codec

        fuzzed =
            Fuzz.examples 1 fuzzer

        randomOverrides =
            [ IR.Random.override "name" IR.string (Random.uniform "Ed" [ "Mario", "Leonardo", "Jeroen" ])
            , IR.Random.override "heightInCentimetres" IR.float (Random.float 160 196)
            , IR.Random.override "dogName" IR.string (Random.uniform "Fido" [ "Kevin", "Rover", "Fifi" ])
            , IR.Random.override "series" IR.char (Random.uniform 'A' (List.range 66 90 |> List.map Char.fromCode))
            , IR.Random.override "model" IR.int (Random.uniform 1000 (List.range 2 5 |> List.map (\n -> n * 1000)))
            ]

        randomGenerator =
            IR.Random.generatorWithOverrides randomOverrides codec

        firstValue =
            Random.step randomGenerator (Random.initialSeed seed1)
                |> Tuple.first

        secondValue =
            Random.step randomGenerator (Random.initialSeed seed2)
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
        , Html.button [ Html.Events.onClick Clicked ] [ Html.text "Click to regenerate!" ]
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
        , Html.code [ Html.Attributes.class "withoutSpaces" ] [ Html.text printed ]
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
    Html.h2 [] [ Html.text txt ]


show : a -> Html.Html msg
show a =
    Html.code [] [ Html.text (Debug.toString a) ]
