module IRTest exposing (..)

import Expect
import Fuzz
import IR
import IR.Adapter
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
        , roundTrip (IR.string |> IR.label "fuzz-override") "String"
        , roundTrip (IR.list IR.bool) "List Bool"
        ]


roundTrip : IR.Codec input -> String -> Test
roundTrip codec name =
    fuzz
        (IR.Fuzz.fuzzerWithOverrides
            [ IR.Fuzz.override "fuzz-override" IR.string (Fuzz.stringOfLengthBetween 0 6)
            ]
            codec
        )
        (name ++ " fromInput -> toOutput roundtrip")
    <|
        \rec ->
            rec
                |> IR.Adapter.fromInput codec
                |> IR.Adapter.toOutput codec
                |> Expect.equal (Ok rec)
