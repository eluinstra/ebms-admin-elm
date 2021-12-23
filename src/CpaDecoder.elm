module CpaDecoder exposing (..)

import XmlParser exposing (Node(..), Xml, parse)


getPartyIds : String -> List String
getPartyIds cpa = parse cpa |> mapResult |> xmlQuery [ "CollaborationProtocolAgreement", "PartyInfo", "PartyId" ] |> List.concatMap toPartyId

toPartyId : Node -> List String
toPartyId node =
    case node of
        Element "ns1:PartyId" [{ value }] [Text t] -> [ value ++ ":" ++ t]
        _ -> []

mapResult : Result (List a) Xml -> Node
mapResult r =
    case r of
        Ok xml ->
            xml.root

        Err _ ->
            Text ""


xmlQuery : List String -> Node -> List Node
xmlQuery elements node =
    case node of
        Element name _ children ->
            case List.head elements of
                Just h ->
                    if String.endsWith h name then
                        case List.tail elements of
                            Just t ->
                                if t == [] then
                                    [node]

                                else
                                    List.concatMap (\child -> xmlQuery t child) children

                            Nothing ->
                                []

                    else
                        []

                Nothing ->
                    []

        _ ->
            []
