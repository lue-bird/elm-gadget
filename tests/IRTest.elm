module IRTest exposing (..)

import Expect
import Fuzz
import IR
import IR.Fuzz
import Test exposing (..)
import TestHelpers exposing (..)


irTests : Test
irTests =
    Test.describe "IR"
        [ roundTrip recordCodec "Record"
        , roundTrip IR.int "Int"
        , roundTrip IR.float "Float"
        , roundTrip IR.char "Char"
        , roundTrip (IR.string |> IR.label "hello world") "String"
        , roundTrip (IR.list IR.bool) "List Bool"
        ]


roundTrip : IR.Codec input -> String -> Test
roundTrip codec name =
    fuzz (IR.Fuzz.fuzzerWithOverrides [ IR.Fuzz.override "hello world" IR.string (Fuzz.constant "hello world") ] codec) (name ++ " fromInput -> toOutput roundtrip") <|
        \rec ->
            rec
                |> IR.fromInput codec
                |> IR.toOutput codec
                |> Expect.equal (Ok rec)
