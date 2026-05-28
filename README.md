# elm-gadget

## What?

Create Gadgets that convert between Elm data types and a generic intermediate
representation (IR)

## Why?

Converting to IR makes it relatively easy to build cool tools (JSON
encoders/decoders, fuzzers, random generators, diff/patchers, parser/printers,
and any other bidirectional converters) for any Elm data type, with minimal
boilerplate.

## Show me!

Do `npx run-pty run-pty.json` in the project root folder.

## How?

```elm
import Gadget
import Gadget.Adapter.Fuzz
import Gadget.Adapter.Json
import Fuzz
import Json.Decode 

-- For a data type like this:

type alias Person = 
    { name : String
    , age : Int 
    }

input = 
    { name = "Ed"
    , age = 44 
    }

-- Just write a Gadget like this:

gadget : Gadget.Gadget Person
gadget = 
    Gadget.record Person
        |> Gadget.field .name Gadget.string 
        |> Gadget.field .age Gadget.int
        |> Gadget.endRecord

-- Now we just need to write a JSON encoder and 
-- decoder for Gadgets, and we'll be able to 
-- use it to convert our Person type to and 
-- from JSON. Here's a JSON adapter I made earlier:

json = 
    Gadget.Adapter.Json.encode gadget input

json --: Json.Decode.Value

decoded = 
    Json.Decode.decodeValue (Gadget.Adapter.Json.decoder gadget) json

decoded --> Ok input

-- The best part is, we can use the exact same Gadget 
-- for other things too. Say we want a fuzzer 
-- for testing: we just write a fuzzer for Gadgets and we 
-- can use it with any Gadget:

fuzzed = 
    Fuzz.examples 1 (Gadget.Adapter.Fuzz.fuzzer gadget)

fuzzed --> [ { age = 92, name = "o \n\\" } ]
```
