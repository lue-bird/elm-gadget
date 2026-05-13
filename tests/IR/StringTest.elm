module IR.StringTest exposing (..)

import Expect
import IR.Advanced as IR
import IR.Fuzz
import IR.String
import Parser
import Test exposing (..)
import TestHelpers exposing (..)


diffTests : Test
diffTests =
    Test.describe "IR.String"
        [ roundTrip recordCodec "Record"
        , roundTrip IR.int "Int"
        , roundTrip IR.float "Float"
        , roundTrip IR.char "Char"
        , roundTrip (IR.string |> IR.label "String") "String"
        , roundTrip (IR.list IR.bool) "List Bool"
        ]


roundTrip : IR.Codec b b -> String -> Test
roundTrip codec name =
    fuzz
        (IR.Fuzz.fuzzer codec)
        (name ++ " print -> parse roundtrip")
    <|
        \val ->
            let
                printed =
                    IR.String.print codec val
            in
            Parser.run (IR.String.parser codec) printed
                |> Expect.equal (Ok val)
