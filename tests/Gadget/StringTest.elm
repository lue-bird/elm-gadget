module Gadget.StringTest exposing (..)

import Expect
import Gadget
import Gadget.Fuzz
import Gadget.String
import Parser
import Test exposing (..)
import TestHelpers exposing (..)


diffTests : Test
diffTests =
    Test.describe "Gadget.String"
        [ roundTrip recordCodec "Record"
        , roundTrip Gadget.int "Int"
        , roundTrip Gadget.float "Float"
        , roundTrip Gadget.char "Char"
        , roundTrip (Gadget.string |> Gadget.label "String") "String"
        , roundTrip (Gadget.list Gadget.bool) "List Bool"
        ]


roundTrip : Gadget.Gadget b -> String -> Test
roundTrip codec name =
    fuzz
        (Gadget.Fuzz.fuzzer codec)
        (name ++ " print -> parse roundtrip")
    <|
        \val ->
            let
                printed =
                    Gadget.String.print codec val
            in
            Parser.run (Gadget.String.parser codec) printed
                |> Expect.equal (Ok val)
