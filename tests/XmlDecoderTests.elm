module XmlDecoderTests exposing (..)

import Expect exposing (Expectation)
import Fuzz as Fuzz exposing (Fuzzer)
import Http exposing (Expect)
import Test exposing (..)
import Xml.Decode exposing (..)
import XmlParser exposing (Node(..), Xml)


type alias Data =
    { string : String

    -- , string1 : String
    , integers : List Int
    }


type alias Data2 =
    { string : String
    , string1 : String
    , integers : List Int
    }


dataDecoder : Decoder Data2
dataDecoder =
    map3 Data2
        (path [ "path", "to", "string", "value" ] (single string))
        (path [ "path", "to", "string", "value" ] (single (stringAttr "attr")))
        (path [ "path", "to", "int", "values" ] (list int))


pipelineDecoder : Decoder Data
pipelineDecoder =
    succeed Data
        |> requiredPath [ "path", "to", "string", "value" ] (single string)
        |> requiredPath [ "path", "to", "int", "values" ] (list int)


xmlString =
    """<root>
  <path>
    <to>
      <string>
        <value attr="1">SomeString</value>
      </string>
      <int>
        <values>1</values>
        <values>2</values>
      </int>
    </to>
  </path>
</root>"""


testDataDecoder : Test
testDataDecoder =
    test "testDataDecoder" <| \_ -> xmlString |> run dataDecoder |> Expect.equal (Ok { string = "SomeString", string1 = "1", integers = [ 1, 2 ] })


testPipelineDecoder : Test
testPipelineDecoder =
    test "testPipelineDecoder" <| \_ -> xmlString |> run pipelineDecoder |> Expect.equal (Ok { string = "SomeString", integers = [ 1, 2 ] })

