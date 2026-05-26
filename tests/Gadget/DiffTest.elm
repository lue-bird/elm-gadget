module Gadget.DiffTest exposing (..)

import Expect
import Gadget
import Gadget.Diff
import Gadget.Fuzz
import Test exposing (..)
import TestHelpers exposing (..)


diffTests : Test
diffTests =
    Test.describe "Gadget.Diff"
        [ roundTrip recordCodec "Record"
        , roundTrip (Gadget.int |> Gadget.label "label") "Int"
        , roundTrip Gadget.float "Float"
        , roundTrip Gadget.char "Char"
        , roundTrip Gadget.string "String"
        , roundTrip (Gadget.list Gadget.bool) "List Bool"
        ]


roundTrip : Gadget.Gadget b -> String -> Test
roundTrip codec name =
    fuzz2
        (Gadget.Fuzz.fuzzer codec)
        (Gadget.Fuzz.fuzzer codec)
        (name ++ " diff -> patch roundtrip")
    <|
        \old new ->
            let
                diff =
                    Gadget.Diff.diff codec old new
            in
            Gadget.Diff.patch codec diff old
                |> Expect.equal (Ok new)
