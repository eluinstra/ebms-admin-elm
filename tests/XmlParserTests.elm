module XmlParserTests exposing (..)

import CpaDecoder exposing (..)
import Expect exposing (Expectation)
import Fuzz as Fuzz exposing (Fuzzer)
import Http exposing (Expect)
import Test exposing (..)
import XmlParser as Parser exposing (Node(..), Xml, parse)


type alias Data =
    { string : String
    , integers : List Int
    }


xmlString =
    """<root><path><to><string><value attr="1">SomeString</value></string><int><values>1</values><values>2</values></int></to></path></root>"""


aString =
    """<a name="value">foo</a>"""


aDecoder : Node -> Maybe String
aDecoder node =
    case node of
        Element "a" _ [ Text t ] ->
            Just t

        _ ->
            Nothing



xmlDecoder : Node -> List (Maybe String)
xmlDecoder node =
    case node of
        Element "root" _ [ Element "path" _ [ Element "to" _ [ Element "string" _ [ Element "value" _ [ Text t ] ] ] ] ] -> [(Just t)]

        _ ->
            []


testParser : Test
testParser =
    test "test1" <| \_ -> parse aString |> Expect.equal (Ok { processingInstructions = [], docType = Nothing, root = Element "a" [ { name = "name", value = "value" } ] [ Text "foo" ] })


testADecoder : Test
testADecoder =
    test "aDecoder" <| \_ -> parse aString |> mapResult |> aDecoder |> Expect.equal (Just "foo")



-- testXmlDecoder : Test
-- testXmlDecoder =
--     test "xmlDecoder" <| \_ -> parse xmlString |> mapResult |> xmlDecoder |> Expect.equal [ Just "SomeString" ]


testXmlQuery : Test
testXmlQuery =
    test "xmlQuery" <| \_ -> parse xmlString |> mapResult |> xmlQuery [ "root", "path", "to", "string", "value" ] |> Expect.equal [ Element "value" [ { name = "attr", value = "1" } ] [ Text "SomeString" ] ]


testXmlQuery1 : Test
testXmlQuery1 =
    test "xmlQuery1" <| \_ -> parse xmlString |> mapResult |> xmlQuery [ "root", "path", "to", "int", "values" ] |> Expect.equal [ Element "values" [] [ Text "1" ], Element "values" [] [ Text "2" ] ]
