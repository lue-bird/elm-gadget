module Gadget.JsonTest exposing (..)

import Expect
import Gadget
import Gadget.Fuzz
import Gadget.Json
import Json.Decode
import Test exposing (..)
import TestHelpers exposing (..)


diffTests : Test
diffTests =
    Test.describe "Gadget.Json"
        [ roundTrip recordCodec "Record"
        , roundTrip Gadget.int "Int"
        , roundTrip Gadget.float "Float"
        , roundTrip Gadget.char "Char"
        , roundTrip (Gadget.string |> Gadget.label "string") "String"
        , roundTrip (Gadget.list Gadget.bool) "List Bool"
        ]


roundTrip : Gadget.Gadget b -> String -> Test
roundTrip codec name =
    fuzz
        (Gadget.Fuzz.fuzzer codec)
        (name ++ " encode -> decode roundtrip")
    <|
        \value ->
            let
                encoded =
                    Gadget.Json.encode codec value
            in
            Json.Decode.decodeValue (Gadget.Json.decoder codec) encoded
                |> Expect.equal (Ok value)
