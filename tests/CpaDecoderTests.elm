module CpaDecoderTests exposing (..)

import CpaDecoder exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import XmlParser exposing (Node(..), Xml, parse)


-- formatResult : Result (List a) Xml -> String
-- formatResult r =
--     case r of
--        Ok xml -> Parser.format xml
--        _ -> ""

-- testXmlFormat : Test
-- testXmlFormat =
--     test "xmlFormat" <| \_ -> parse cpa |> formatResult |> Expect.equal ""


testXmlQuery : Test
testXmlQuery =
    test "xmlQuery" <| \_ -> parse cpa |> mapResult |> xmlQuery [ "CollaborationProtocolAgreement", "PartyInfo", "PartyId" ] |> Expect.equal [Element "ns1:PartyId" [{ name = "ns1:type", value = "urn:osb:oin" }] [Text "00000000000000000000"],Element "ns1:PartyId" [{ name = "ns1:type", value = "urn:osb:oin" }] [Text "00000000000000000001"]]


testGetPartyIds : Test
testGetPartyIds =
    test "getPartyIds" <| \_ -> getPartyIds cpa |> Expect.equal [ "urn:osb:oin:00000000000000000000","urn:osb:oin:00000000000000000001" ]

cpa : String
cpa = """<?xml version='1.0' encoding='UTF-8'?>
<ns1:CollaborationProtocolAgreement xmlns:ns2="http://www.w3.org/1999/xlink" xmlns:ns1="http://www.oasis-open.org/committees/ebxml-cppa/schema/cpp-cpa-2_0.xsd" xmlns:ns3="http://www.w3.org/2000/09/xmldsig#" ns1:cpaid="cpaStubEBF.rm.https.signed" ns1:version="1.0">
  <ns1:Status ns1:value="agreed"/>
  <ns1:Start>2011-01-01T01:00:00+01:00</ns1:Start>
  <ns1:End>2031-01-01T01:00:00+01:00</ns1:End>
  <ns1:PartyInfo ns1:partyName="Logius" ns1:defaultMshChannelId="DIGIPOORT_defaultDeliveryChannel_ProfileBestEffortSigned" ns1:defaultMshPackageId="MshSignalPackage">
    <ns1:PartyId ns1:type="urn:osb:oin">00000000000000000000</ns1:PartyId>
    <ns1:PartyRef ns2:href=""/>
    <ns1:CollaborationRole>
      <ns1:ProcessSpecification ns1:name="afleveren" ns1:version="1.0" ns1:uuid="urn:overheidsservicebus.nl:osr:Afleveren:OsbAfleveren11$10" ns2:href="http://www.overheidsservicebus.nl/osr/Afleveren"/>
      <ns1:Role ns1:name="DIGIPOORT" ns2:href="http://www.overheidsservicebus.nl/osr/"/>
      <ns1:ServiceBinding>
        <ns1:Service ns1:type="urn:osb:services">osb:afleveren:1.1$1.0</ns1:Service>
        <ns1:CanSend>
          <ns1:ThisPartyActionBinding ns1:id="DIGIPOORT_S_Afleveren" ns1:action="afleveren" ns1:packageId="XMLMessagePackage">
            <ns1:BusinessTransactionCharacteristics ns1:isNonRepudiationRequired="true" ns1:isNonRepudiationReceiptRequired="true" ns1:isConfidential="transient" ns1:isAuthenticated="transient-and-persistent" ns1:isTamperProof="transient-and-persistent" ns1:isAuthorizationRequired="true" ns1:isIntelligibleCheckRequired="false" ns1:timeToPerform="P0Y0M2DT0H0M0.000S"/>
            <ns1:ChannelId>DIGIPOORT_defaultDeliveryChannel_ProfileReliableMessagingSigned</ns1:ChannelId>
          </ns1:ThisPartyActionBinding>
          <ns1:OtherPartyActionBinding>OVERHEID_R_Afleveren</ns1:OtherPartyActionBinding>
        </ns1:CanSend>
        <ns1:CanReceive>
          <ns1:ThisPartyActionBinding ns1:id="DIGIPOORT_R_BevestigAfleveren" ns1:action="bevestigAfleveren" ns1:packageId="XMLMessagePackage">
            <ns1:BusinessTransactionCharacteristics ns1:isNonRepudiationRequired="true" ns1:isNonRepudiationReceiptRequired="true" ns1:isConfidential="transient" ns1:isAuthenticated="transient-and-persistent" ns1:isTamperProof="transient-and-persistent" ns1:isAuthorizationRequired="true" ns1:isIntelligibleCheckRequired="false" ns1:timeToPerform="P0Y0M2DT0H0M0.000S"/>
            <ns1:ChannelId>DIGIPOORT_defaultDeliveryChannel_ProfileReliableMessagingSigned</ns1:ChannelId>
          </ns1:ThisPartyActionBinding>
          <ns1:OtherPartyActionBinding>OVERHEID_S_BevestigAfleveren</ns1:OtherPartyActionBinding>
        </ns1:CanReceive>
      </ns1:ServiceBinding>
    </ns1:CollaborationRole>
    <ns1:CollaborationRole>
      <ns1:ProcessSpecification ns1:name="aanleveren" ns1:version="1.0" ns1:uuid="urn:overheidsservicebus.nl:osr:Aanleveren:OsbAanleveren10$10" ns2:href="http://www.overheidsservicebus.nl/osr/Aanleveren"/>
      <ns1:Role ns1:name="DIGIPOORT" ns2:href="http://www.overheidsservicebus.nl/osr/"/>
      <ns1:ServiceBinding>
        <ns1:Service ns1:type="urn:osb:services">osb:aanleveren:1.1$1.0</ns1:Service>
        <ns1:CanSend>
          <ns1:ThisPartyActionBinding ns1:id="DIGIPOORT_S_BevestigAanleveren" ns1:action="bevestigAanleveren" ns1:packageId="XMLMessagePackage">
            <ns1:BusinessTransactionCharacteristics ns1:isNonRepudiationRequired="true" ns1:isNonRepudiationReceiptRequired="true" ns1:isConfidential="transient" ns1:isAuthenticated="transient-and-persistent" ns1:isTamperProof="transient-and-persistent" ns1:isAuthorizationRequired="true" ns1:isIntelligibleCheckRequired="false" ns1:timeToPerform="P0Y0M2DT0H0M0.000S"/>
            <ns1:ChannelId>DIGIPOORT_defaultDeliveryChannel_ProfileReliableMessagingSigned</ns1:ChannelId>
          </ns1:ThisPartyActionBinding>
          <ns1:OtherPartyActionBinding>OVERHEID_R_BevestigAanleveren</ns1:OtherPartyActionBinding>
        </ns1:CanSend>
        <ns1:CanReceive>
          <ns1:ThisPartyActionBinding ns1:id="DIGIPOORT_R_Aanleveren" ns1:action="aanleveren" ns1:packageId="XMLMessagePackage">
            <ns1:BusinessTransactionCharacteristics ns1:isNonRepudiationRequired="true" ns1:isNonRepudiationReceiptRequired="true" ns1:isConfidential="transient" ns1:isAuthenticated="transient-and-persistent" ns1:isTamperProof="transient-and-persistent" ns1:isAuthorizationRequired="true" ns1:isIntelligibleCheckRequired="false" ns1:timeToPerform="P0Y0M2DT0H0M0.000S"/>
            <ns1:ChannelId>DIGIPOORT_defaultDeliveryChannel_ProfileReliableMessagingSigned</ns1:ChannelId>
          </ns1:ThisPartyActionBinding>
          <ns1:OtherPartyActionBinding>OVERHEID_S_Aanleveren</ns1:OtherPartyActionBinding>
        </ns1:CanReceive>
      </ns1:ServiceBinding>
    </ns1:CollaborationRole>
    <ns1:Certificate ns1:certId="DIGIPOORT_SigningCert">
      <ns3:KeyInfo>
        <ns3:KeyValue>
          <ns3:RSAKeyValue>
            <ns3:Modulus>jOtmODIw7Qv3QOVMyIUbpgT0HDrHBynE2LfjAZk7bNfZrOtNdAq65mIDqUEn8SRsY2zBEmPHxgf38nmJcpcOIORApFDxZMZd44rbrghq+AutKr36krDvLl4kFiUFRefl7IjhyKFwZvH2bK3odwDNIExy285amVduCkG7bPkBcTwOVJbAYR9A2zXJHDZti2fvWMZpEm3B0M7LPJ6KfwCBmPDWXIiqqXIAeOWZjAC4UuqUm6jzkcs9tfO2Qm9bvJZdceeJcWMC90AouL5f8iR3aRt4rHQQZNknmNxTrpqE4G4eTBau9ix9F3iyV15QWgYQLZR7Q2kfZwCV7Zp4JgLYcQ==</ns3:Modulus>
            <ns3:Exponent>AQAB</ns3:Exponent>
          </ns3:RSAKeyValue>
        </ns3:KeyValue>
        <ns3:X509Data>
          <ns3:X509SubjectName>CN=localhost, OU=Rules Matter, O=Ordina, L=Groningen, ST=Groningen, C=NL</ns3:X509SubjectName>
          <ns3:X509IssuerSerial>
            <ns3:X509IssuerName>CN=localhost, OU=Rules Matter, O=Ordina, L=Groningen, ST=Groningen, C=NL</ns3:X509IssuerName>
            <ns3:X509SerialNumber>1336114976</ns3:X509SerialNumber>
          </ns3:X509IssuerSerial>
          <ns3:X509Certificate>MIIDlzCCAn+gAwIBAgIET6N/IDANBgkqhkiG9w0BAQsFADBxMQswCQYDVQQGEwJOTDESMBAGA1UECBMJR3JvbmluZ2VuMRIwEAYDVQQHEwlHcm9uaW5nZW4xDzANBgNVBAoTBk9yZGluYTEVMBMGA1UECxMMUnVsZXMgTWF0dGVyMRIwEAYDVQQDEwlsb2NhbGhvc3QwHhcNMjAwNjA3MTQyOTU5WhcNMzAwNjA1MTQyOTU5WjBxMQswCQYDVQQGEwJOTDESMBAGA1UECBMJR3JvbmluZ2VuMRIwEAYDVQQHEwlHcm9uaW5nZW4xDzANBgNVBAoTBk9yZGluYTEVMBMGA1UECxMMUnVsZXMgTWF0dGVyMRIwEAYDVQQDEwlsb2NhbGhvc3QwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCM62Y4MjDtC/dA5UzIhRumBPQcOscHKcTYt+MBmTts19ms6010CrrmYgOpQSfxJGxjbMESY8fGB/fyeYlylw4g5ECkUPFkxl3jituuCGr4C60qvfqSsO8uXiQWJQVF5+XsiOHIoXBm8fZsreh3AM0gTHLbzlqZV24KQbts+QFxPA5UlsBhH0DbNckcNm2LZ+9YxmkSbcHQzss8nop/AIGY8NZciKqpcgB45ZmMALhS6pSbqPORyz2187ZCb1u8ll1x54lxYwL3QCi4vl/yJHdpG3isdBBk2SeY3FOumoTgbh5MFq72LH0XeLJXXlBaBhAtlHtDaR9nAJXtmngmAthxAgMBAAGjNzA1MBQGA1UdEQQNMAuCCWxvY2FsaG9zdDAdBgNVHQ4EFgQUIGrhAcn6OsrmULDTpbcHhS31ezcwDQYJKoZIhvcNAQELBQADggEBABsqfBJfepASqhu7m7HU97k4aCYiElrJEcyhUluUmAfV/sKo2zD1EtziE4zdQMBHICOYkBLIIvNEpIrYXhsTWOymwjIp+niz8cDYw8Ya57RPNs4cQHZXEnOqwpqal2n9WGIq7Op4OkRT8thQisua/x2zI+N1WzM+MOu5Z/mKRRKQDdWTvNnWpHNp5JZ1TAGbZxHoUUQDQNwEXCMOuzXdE6x8rVLwfEMwpievFnIn9DwegzXM4GBBe+fHErox1WA+ijrcNjl4zhFQ1himdCuEY6AQeAEd5wAalQJ34FpZPfcWRHXRSkUszoiIGArhX0fmL3GeZq+a4tN7XCBNaTPqib4=</ns3:X509Certificate>
        </ns3:X509Data>
      </ns3:KeyInfo>
    </ns1:Certificate>
    <ns1:Certificate ns1:certId="DIGIPOORT_EncryptionCert">
      <ns3:KeyInfo>
        <ns3:KeyValue>
          <ns3:RSAKeyValue>
            <ns3:Modulus>jOtmODIw7Qv3QOVMyIUbpgT0HDrHBynE2LfjAZk7bNfZrOtNdAq65mIDqUEn8SRsY2zBEmPHxgf38nmJcpcOIORApFDxZMZd44rbrghq+AutKr36krDvLl4kFiUFRefl7IjhyKFwZvH2bK3odwDNIExy285amVduCkG7bPkBcTwOVJbAYR9A2zXJHDZti2fvWMZpEm3B0M7LPJ6KfwCBmPDWXIiqqXIAeOWZjAC4UuqUm6jzkcs9tfO2Qm9bvJZdceeJcWMC90AouL5f8iR3aRt4rHQQZNknmNxTrpqE4G4eTBau9ix9F3iyV15QWgYQLZR7Q2kfZwCV7Zp4JgLYcQ==</ns3:Modulus>
            <ns3:Exponent>AQAB</ns3:Exponent>
          </ns3:RSAKeyValue>
        </ns3:KeyValue>
        <ns3:X509Data>
          <ns3:X509SubjectName>CN=localhost, OU=Rules Matter, O=Ordina, L=Groningen, ST=Groningen, C=NL</ns3:X509SubjectName>
          <ns3:X509IssuerSerial>
            <ns3:X509IssuerName>CN=localhost, OU=Rules Matter, O=Ordina, L=Groningen, ST=Groningen, C=NL</ns3:X509IssuerName>
            <ns3:X509SerialNumber>1336114976</ns3:X509SerialNumber>
          </ns3:X509IssuerSerial>
          <ns3:X509Certificate>MIIDlzCCAn+gAwIBAgIET6N/IDANBgkqhkiG9w0BAQsFADBxMQswCQYDVQQGEwJOTDESMBAGA1UECBMJR3JvbmluZ2VuMRIwEAYDVQQHEwlHcm9uaW5nZW4xDzANBgNVBAoTBk9yZGluYTEVMBMGA1UECxMMUnVsZXMgTWF0dGVyMRIwEAYDVQQDEwlsb2NhbGhvc3QwHhcNMjAwNjA3MTQyOTU5WhcNMzAwNjA1MTQyOTU5WjBxMQswCQYDVQQGEwJOTDESMBAGA1UECBMJR3JvbmluZ2VuMRIwEAYDVQQHEwlHcm9uaW5nZW4xDzANBgNVBAoTBk9yZGluYTEVMBMGA1UECxMMUnVsZXMgTWF0dGVyMRIwEAYDVQQDEwlsb2NhbGhvc3QwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCM62Y4MjDtC/dA5UzIhRumBPQcOscHKcTYt+MBmTts19ms6010CrrmYgOpQSfxJGxjbMESY8fGB/fyeYlylw4g5ECkUPFkxl3jituuCGr4C60qvfqSsO8uXiQWJQVF5+XsiOHIoXBm8fZsreh3AM0gTHLbzlqZV24KQbts+QFxPA5UlsBhH0DbNckcNm2LZ+9YxmkSbcHQzss8nop/AIGY8NZciKqpcgB45ZmMALhS6pSbqPORyz2187ZCb1u8ll1x54lxYwL3QCi4vl/yJHdpG3isdBBk2SeY3FOumoTgbh5MFq72LH0XeLJXXlBaBhAtlHtDaR9nAJXtmngmAthxAgMBAAGjNzA1MBQGA1UdEQQNMAuCCWxvY2FsaG9zdDAdBgNVHQ4EFgQUIGrhAcn6OsrmULDTpbcHhS31ezcwDQYJKoZIhvcNAQELBQADggEBABsqfBJfepASqhu7m7HU97k4aCYiElrJEcyhUluUmAfV/sKo2zD1EtziE4zdQMBHICOYkBLIIvNEpIrYXhsTWOymwjIp+niz8cDYw8Ya57RPNs4cQHZXEnOqwpqal2n9WGIq7Op4OkRT8thQisua/x2zI+N1WzM+MOu5Z/mKRRKQDdWTvNnWpHNp5JZ1TAGbZxHoUUQDQNwEXCMOuzXdE6x8rVLwfEMwpievFnIn9DwegzXM4GBBe+fHErox1WA+ijrcNjl4zhFQ1himdCuEY6AQeAEd5wAalQJ34FpZPfcWRHXRSkUszoiIGArhX0fmL3GeZq+a4tN7XCBNaTPqib4=</ns3:X509Certificate>
        </ns3:X509Data>
      </ns3:KeyInfo>
    </ns1:Certificate>
    <ns1:Certificate ns1:certId="DIGIPOORT_ServerCert">
      <ns3:KeyInfo>
        <ns3:KeyValue>
          <ns3:RSAKeyValue>
            <ns3:Modulus>jOtmODIw7Qv3QOVMyIUbpgT0HDrHBynE2LfjAZk7bNfZrOtNdAq65mIDqUEn8SRsY2zBEmPHxgf38nmJcpcOIORApFDxZMZd44rbrghq+AutKr36krDvLl4kFiUFRefl7IjhyKFwZvH2bK3odwDNIExy285amVduCkG7bPkBcTwOVJbAYR9A2zXJHDZti2fvWMZpEm3B0M7LPJ6KfwCBmPDWXIiqqXIAeOWZjAC4UuqUm6jzkcs9tfO2Qm9bvJZdceeJcWMC90AouL5f8iR3aRt4rHQQZNknmNxTrpqE4G4eTBau9ix9F3iyV15QWgYQLZR7Q2kfZwCV7Zp4JgLYcQ==</ns3:Modulus>
            <ns3:Exponent>AQAB</ns3:Exponent>
          </ns3:RSAKeyValue>
        </ns3:KeyValue>
        <ns3:X509Data>
          <ns3:X509SubjectName>CN=localhost, OU=Rules Matter, O=Ordina, L=Groningen, ST=Groningen, C=NL</ns3:X509SubjectName>
          <ns3:X509IssuerSerial>
            <ns3:X509IssuerName>CN=localhost, OU=Rules Matter, O=Ordina, L=Groningen, ST=Groningen, C=NL</ns3:X509IssuerName>
            <ns3:X509SerialNumber>1336114976</ns3:X509SerialNumber>
          </ns3:X509IssuerSerial>
          <ns3:X509Certificate>MIIDlzCCAn+gAwIBAgIET6N/IDANBgkqhkiG9w0BAQsFADBxMQswCQYDVQQGEwJOTDESMBAGA1UECBMJR3JvbmluZ2VuMRIwEAYDVQQHEwlHcm9uaW5nZW4xDzANBgNVBAoTBk9yZGluYTEVMBMGA1UECxMMUnVsZXMgTWF0dGVyMRIwEAYDVQQDEwlsb2NhbGhvc3QwHhcNMjAwNjA3MTQyOTU5WhcNMzAwNjA1MTQyOTU5WjBxMQswCQYDVQQGEwJOTDESMBAGA1UECBMJR3JvbmluZ2VuMRIwEAYDVQQHEwlHcm9uaW5nZW4xDzANBgNVBAoTBk9yZGluYTEVMBMGA1UECxMMUnVsZXMgTWF0dGVyMRIwEAYDVQQDEwlsb2NhbGhvc3QwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCM62Y4MjDtC/dA5UzIhRumBPQcOscHKcTYt+MBmTts19ms6010CrrmYgOpQSfxJGxjbMESY8fGB/fyeYlylw4g5ECkUPFkxl3jituuCGr4C60qvfqSsO8uXiQWJQVF5+XsiOHIoXBm8fZsreh3AM0gTHLbzlqZV24KQbts+QFxPA5UlsBhH0DbNckcNm2LZ+9YxmkSbcHQzss8nop/AIGY8NZciKqpcgB45ZmMALhS6pSbqPORyz2187ZCb1u8ll1x54lxYwL3QCi4vl/yJHdpG3isdBBk2SeY3FOumoTgbh5MFq72LH0XeLJXXlBaBhAtlHtDaR9nAJXtmngmAthxAgMBAAGjNzA1MBQGA1UdEQQNMAuCCWxvY2FsaG9zdDAdBgNVHQ4EFgQUIGrhAcn6OsrmULDTpbcHhS31ezcwDQYJKoZIhvcNAQELBQADggEBABsqfBJfepASqhu7m7HU97k4aCYiElrJEcyhUluUmAfV/sKo2zD1EtziE4zdQMBHICOYkBLIIvNEpIrYXhsTWOymwjIp+niz8cDYw8Ya57RPNs4cQHZXEnOqwpqal2n9WGIq7Op4OkRT8thQisua/x2zI+N1WzM+MOu5Z/mKRRKQDdWTvNnWpHNp5JZ1TAGbZxHoUUQDQNwEXCMOuzXdE6x8rVLwfEMwpievFnIn9DwegzXM4GBBe+fHErox1WA+ijrcNjl4zhFQ1himdCuEY6AQeAEd5wAalQJ34FpZPfcWRHXRSkUszoiIGArhX0fmL3GeZq+a4tN7XCBNaTPqib4=</ns3:X509Certificate>
        </ns3:X509Data>
      </ns3:KeyInfo>
    </ns1:Certificate>
    <ns1:Certificate ns1:certId="DIGIPOORT_ClientCert">
      <ns3:KeyInfo>
        <ns3:KeyValue>
          <ns3:RSAKeyValue>
            <ns3:Modulus>jOtmODIw7Qv3QOVMyIUbpgT0HDrHBynE2LfjAZk7bNfZrOtNdAq65mIDqUEn8SRsY2zBEmPHxgf38nmJcpcOIORApFDxZMZd44rbrghq+AutKr36krDvLl4kFiUFRefl7IjhyKFwZvH2bK3odwDNIExy285amVduCkG7bPkBcTwOVJbAYR9A2zXJHDZti2fvWMZpEm3B0M7LPJ6KfwCBmPDWXIiqqXIAeOWZjAC4UuqUm6jzkcs9tfO2Qm9bvJZdceeJcWMC90AouL5f8iR3aRt4rHQQZNknmNxTrpqE4G4eTBau9ix9F3iyV15QWgYQLZR7Q2kfZwCV7Zp4JgLYcQ==</ns3:Modulus>
            <ns3:Exponent>AQAB</ns3:Exponent>
          </ns3:RSAKeyValue>
        </ns3:KeyValue>
        <ns3:X509Data>
          <ns3:X509SubjectName>CN=localhost, OU=Rules Matter, O=Ordina, L=Groningen, ST=Groningen, C=NL</ns3:X509SubjectName>
          <ns3:X509IssuerSerial>
            <ns3:X509IssuerName>CN=localhost, OU=Rules Matter, O=Ordina, L=Groningen, ST=Groningen, C=NL</ns3:X509IssuerName>
            <ns3:X509SerialNumber>1336114976</ns3:X509SerialNumber>
          </ns3:X509IssuerSerial>
          <ns3:X509Certificate>MIIDlzCCAn+gAwIBAgIET6N/IDANBgkqhkiG9w0BAQsFADBxMQswCQYDVQQGEwJOTDESMBAGA1UECBMJR3JvbmluZ2VuMRIwEAYDVQQHEwlHcm9uaW5nZW4xDzANBgNVBAoTBk9yZGluYTEVMBMGA1UECxMMUnVsZXMgTWF0dGVyMRIwEAYDVQQDEwlsb2NhbGhvc3QwHhcNMjAwNjA3MTQyOTU5WhcNMzAwNjA1MTQyOTU5WjBxMQswCQYDVQQGEwJOTDESMBAGA1UECBMJR3JvbmluZ2VuMRIwEAYDVQQHEwlHcm9uaW5nZW4xDzANBgNVBAoTBk9yZGluYTEVMBMGA1UECxMMUnVsZXMgTWF0dGVyMRIwEAYDVQQDEwlsb2NhbGhvc3QwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCM62Y4MjDtC/dA5UzIhRumBPQcOscHKcTYt+MBmTts19ms6010CrrmYgOpQSfxJGxjbMESY8fGB/fyeYlylw4g5ECkUPFkxl3jituuCGr4C60qvfqSsO8uXiQWJQVF5+XsiOHIoXBm8fZsreh3AM0gTHLbzlqZV24KQbts+QFxPA5UlsBhH0DbNckcNm2LZ+9YxmkSbcHQzss8nop/AIGY8NZciKqpcgB45ZmMALhS6pSbqPORyz2187ZCb1u8ll1x54lxYwL3QCi4vl/yJHdpG3isdBBk2SeY3FOumoTgbh5MFq72LH0XeLJXXlBaBhAtlHtDaR9nAJXtmngmAthxAgMBAAGjNzA1MBQGA1UdEQQNMAuCCWxvY2FsaG9zdDAdBgNVHQ4EFgQUIGrhAcn6OsrmULDTpbcHhS31ezcwDQYJKoZIhvcNAQELBQADggEBABsqfBJfepASqhu7m7HU97k4aCYiElrJEcyhUluUmAfV/sKo2zD1EtziE4zdQMBHICOYkBLIIvNEpIrYXhsTWOymwjIp+niz8cDYw8Ya57RPNs4cQHZXEnOqwpqal2n9WGIq7Op4OkRT8thQisua/x2zI+N1WzM+MOu5Z/mKRRKQDdWTvNnWpHNp5JZ1TAGbZxHoUUQDQNwEXCMOuzXdE6x8rVLwfEMwpievFnIn9DwegzXM4GBBe+fHErox1WA+ijrcNjl4zhFQ1himdCuEY6AQeAEd5wAalQJ34FpZPfcWRHXRSkUszoiIGArhX0fmL3GeZq+a4tN7XCBNaTPqib4=</ns3:X509Certificate>
        </ns3:X509Data>
      </ns3:KeyInfo>
    </ns1:Certificate>
    <ns1:SecurityDetails ns1:securityId="DIGIPOORT_Security">
      <ns1:TrustAnchors>
        <ns1:AnchorCertificateRef ns1:certId="DIGIPOORT_SigningCert"/>
        <ns1:AnchorCertificateRef ns1:certId="DIGIPOORT_EncryptionCert"/>
      </ns1:TrustAnchors>
    </ns1:SecurityDetails>
    <ns1:SecurityDetails ns1:securityId="DIGIPOORT_TransportSecurity">
      <ns1:TrustAnchors>
        <ns1:AnchorCertificateRef ns1:certId="DIGIPOORT_ServerCert"/>
        <ns1:AnchorCertificateRef ns1:certId="DIGIPOORT_ClientCert"/>
      </ns1:TrustAnchors>
    </ns1:SecurityDetails>
    <ns1:DeliveryChannel ns1:channelId="DIGIPOORT_defaultDeliveryChannel_ProfileBestEffortSigned" ns1:transportId="DIGIPOORT_transport_HTTPS" ns1:docExchangeId="DIGIPOORT_SIGNED_BestEffort">
      <ns1:MessagingCharacteristics ns1:syncReplyMode="none" ns1:ackRequested="never" ns1:ackSignatureRequested="never" ns1:duplicateElimination="never" ns1:actor="urn:oasis:names:tc:ebxml-msg:actor:toPartyMSH"/>
    </ns1:DeliveryChannel>
    <ns1:DeliveryChannel ns1:channelId="DIGIPOORT_defaultDeliveryChannel_ProfileReliableMessagingSigned" ns1:transportId="DIGIPOORT_transport_HTTPS" ns1:docExchangeId="DIGIPOORT_SIGNED_ReliableMessaging">
      <ns1:MessagingCharacteristics ns1:syncReplyMode="none" ns1:ackRequested="always" ns1:ackSignatureRequested="never" ns1:duplicateElimination="always" ns1:actor="urn:oasis:names:tc:ebxml-msg:actor:toPartyMSH"/>
    </ns1:DeliveryChannel>
    <ns1:Transport ns1:transportId="DIGIPOORT_transport_HTTPS">
      <ns1:TransportSender>
        <ns1:TransportProtocol ns1:version="1.1">HTTP</ns1:TransportProtocol>
        <ns1:TransportClientSecurity>
          <ns1:TransportSecurityProtocol ns1:version="3.0">SSL</ns1:TransportSecurityProtocol>
          <ns1:ClientCertificateRef ns1:certId="DIGIPOORT_ClientCert"/>
          <ns1:ServerSecurityDetailsRef ns1:securityId="DIGIPOORT_TransportSecurity"/>
        </ns1:TransportClientSecurity>
      </ns1:TransportSender>
      <ns1:TransportReceiver>
        <ns1:TransportProtocol ns1:version="1.1">HTTP</ns1:TransportProtocol>
        <ns1:Endpoint ns1:uri="https://localhost:8888/ebms" ns1:type="allPurpose"/>
        <ns1:TransportServerSecurity>
          <ns1:TransportSecurityProtocol ns1:version="3.0">SSL</ns1:TransportSecurityProtocol>
          <ns1:ServerCertificateRef ns1:certId="DIGIPOORT_ServerCert"/>
          <ns1:ClientSecurityDetailsRef ns1:securityId="DIGIPOORT_TransportSecurity"/>
        </ns1:TransportServerSecurity>
      </ns1:TransportReceiver>
    </ns1:Transport>
    <ns1:DocExchange ns1:docExchangeId="DIGIPOORT_SIGNED_BestEffort">
      <ns1:ebXMLSenderBinding ns1:version="2.0">
        <ns1:SenderNonRepudiation>
          <ns1:NonRepudiationProtocol>http://www.w3.org/2000/09/xmldsig#</ns1:NonRepudiationProtocol>
          <ns1:HashFunction>http://www.w3.org/2000/09/xmldsig#sha1</ns1:HashFunction>
          <ns1:SignatureAlgorithm>http://www.w3.org/2000/09/xmldsig#rsa-sha1</ns1:SignatureAlgorithm>
          <ns1:SigningCertificateRef ns1:certId="DIGIPOORT_SigningCert"/>
        </ns1:SenderNonRepudiation>
      </ns1:ebXMLSenderBinding>
      <ns1:ebXMLReceiverBinding ns1:version="2.0">
        <ns1:ReceiverNonRepudiation>
          <ns1:NonRepudiationProtocol>http://www.w3.org/2000/09/xmldsig#</ns1:NonRepudiationProtocol>
          <ns1:HashFunction>http://www.w3.org/2000/09/xmldsig#sha1</ns1:HashFunction>
          <ns1:SignatureAlgorithm>http://www.w3.org/2000/09/xmldsig#rsa-sha1</ns1:SignatureAlgorithm>
          <ns1:SigningSecurityDetailsRef ns1:securityId="DIGIPOORT_Security"/>
        </ns1:ReceiverNonRepudiation>
      </ns1:ebXMLReceiverBinding>
    </ns1:DocExchange>
    <ns1:DocExchange ns1:docExchangeId="DIGIPOORT_SIGNED_ReliableMessaging">
      <ns1:ebXMLSenderBinding ns1:version="2.0">
        <ns1:ReliableMessaging>
          <ns1:Retries>5</ns1:Retries>
          <ns1:RetryInterval>P0Y0M0DT0H5M0.000S</ns1:RetryInterval>
          <ns1:MessageOrderSemantics>NotGuaranteed</ns1:MessageOrderSemantics>
        </ns1:ReliableMessaging>
        <ns1:PersistDuration>P0Y0M0DT0H30M0.000S</ns1:PersistDuration>
        <ns1:SenderNonRepudiation>
          <ns1:NonRepudiationProtocol>http://www.w3.org/2000/09/xmldsig#</ns1:NonRepudiationProtocol>
          <ns1:HashFunction>http://www.w3.org/2000/09/xmldsig#sha1</ns1:HashFunction>
          <ns1:SignatureAlgorithm>http://www.w3.org/2000/09/xmldsig#rsa-sha1</ns1:SignatureAlgorithm>
          <ns1:SigningCertificateRef ns1:certId="DIGIPOORT_SigningCert"/>
        </ns1:SenderNonRepudiation>
      </ns1:ebXMLSenderBinding>
      <ns1:ebXMLReceiverBinding ns1:version="2.0">
        <ns1:ReliableMessaging>
          <ns1:Retries>5</ns1:Retries>
          <ns1:RetryInterval>P0Y0M0DT0H5M0.000S</ns1:RetryInterval>
          <ns1:MessageOrderSemantics>NotGuaranteed</ns1:MessageOrderSemantics>
        </ns1:ReliableMessaging>
        <ns1:PersistDuration>P0Y0M0DT0H30M0.000S</ns1:PersistDuration>
        <ns1:ReceiverNonRepudiation>
          <ns1:NonRepudiationProtocol>http://www.w3.org/2000/09/xmldsig#</ns1:NonRepudiationProtocol>
          <ns1:HashFunction>http://www.w3.org/2000/09/xmldsig#sha1</ns1:HashFunction>
          <ns1:SignatureAlgorithm>http://www.w3.org/2000/09/xmldsig#rsa-sha1</ns1:SignatureAlgorithm>
          <ns1:SigningSecurityDetailsRef ns1:securityId="DIGIPOORT_Security"/>
        </ns1:ReceiverNonRepudiation>
      </ns1:ebXMLReceiverBinding>
    </ns1:DocExchange>
  </ns1:PartyInfo>
  <ns1:PartyInfo ns1:partyName="Overheid" ns1:defaultMshChannelId="OVERHEID_defaultDeliveryChannel_ProfileBestEffortSigned" ns1:defaultMshPackageId="MshSignalPackage">
    <ns1:PartyId ns1:type="urn:osb:oin">00000000000000000001</ns1:PartyId>
    <ns1:PartyRef ns2:href=""/>
    <ns1:CollaborationRole>
      <ns1:ProcessSpecification ns1:name="afleveren" ns1:version="1.0" ns1:uuid="urn:overheidsservicebus.nl:osr:Afleveren:OsbAfleveren11$10" ns2:href="http://www.overheidsservicebus.nl/osr/Afleveren"/>
      <ns1:Role ns1:name="OVERHEID" ns2:href="http://www.overheidsservicebus.nl/osr/"/>
      <ns1:ServiceBinding>
        <ns1:Service ns1:type="urn:osb:services">osb:afleveren:1.1$1.0</ns1:Service>
        <ns1:CanSend>
          <ns1:ThisPartyActionBinding ns1:id="OVERHEID_S_BevestigAfleveren" ns1:action="bevestigAfleveren" ns1:packageId="XMLMessagePackage">
            <ns1:BusinessTransactionCharacteristics ns1:isNonRepudiationRequired="true" ns1:isNonRepudiationReceiptRequired="true" ns1:isConfidential="transient" ns1:isAuthenticated="transient-and-persistent" ns1:isTamperProof="transient-and-persistent" ns1:isAuthorizationRequired="true" ns1:isIntelligibleCheckRequired="false" ns1:timeToPerform="P0Y0M2DT0H0M0.000S"/>
            <ns1:ChannelId>OVERHEID_defaultDeliveryChannel_ProfileReliableMessagingSigned</ns1:ChannelId>
          </ns1:ThisPartyActionBinding>
          <ns1:OtherPartyActionBinding>DIGIPOORT_R_BevestigAfleveren</ns1:OtherPartyActionBinding>
        </ns1:CanSend>
        <ns1:CanReceive>
          <ns1:ThisPartyActionBinding ns1:id="OVERHEID_R_Afleveren" ns1:action="afleveren" ns1:packageId="XMLMessagePackage">
            <ns1:BusinessTransactionCharacteristics ns1:isNonRepudiationRequired="true" ns1:isNonRepudiationReceiptRequired="true" ns1:isConfidential="transient" ns1:isAuthenticated="transient-and-persistent" ns1:isTamperProof="transient-and-persistent" ns1:isAuthorizationRequired="true" ns1:isIntelligibleCheckRequired="false" ns1:timeToPerform="P0Y0M2DT0H0M0.000S"/>
            <ns1:ChannelId>OVERHEID_defaultDeliveryChannel_ProfileReliableMessagingSigned</ns1:ChannelId>
          </ns1:ThisPartyActionBinding>
          <ns1:OtherPartyActionBinding>DIGIPOORT_S_Afleveren</ns1:OtherPartyActionBinding>
        </ns1:CanReceive>
      </ns1:ServiceBinding>
    </ns1:CollaborationRole>
    <ns1:CollaborationRole>
      <ns1:ProcessSpecification ns1:name="aanleveren" ns1:version="1.0" ns1:uuid="urn:overheidsservicebus.nl:osr:Aanleveren:OsbAanleveren10$10" ns2:href="http://www.overheidsservicebus.nl/osr/Aanleveren"/>
      <ns1:Role ns1:name="OVERHEID" ns2:href="http://www.overheidsservicebus.nl/osr/"/>
      <ns1:ServiceBinding>
        <ns1:Service ns1:type="urn:osb:services">osb:aanleveren:1.1$1.0</ns1:Service>
        <ns1:CanSend>
          <ns1:ThisPartyActionBinding ns1:id="OVERHEID_S_Aanleveren" ns1:action="aanleveren" ns1:packageId="XMLMessagePackage">
            <ns1:BusinessTransactionCharacteristics ns1:isNonRepudiationRequired="true" ns1:isNonRepudiationReceiptRequired="true" ns1:isConfidential="transient" ns1:isAuthenticated="transient-and-persistent" ns1:isTamperProof="transient-and-persistent" ns1:isAuthorizationRequired="true" ns1:isIntelligibleCheckRequired="false" ns1:timeToPerform="P0Y0M2DT0H0M0.000S"/>
            <ns1:ChannelId>OVERHEID_defaultDeliveryChannel_ProfileReliableMessagingSigned</ns1:ChannelId>
          </ns1:ThisPartyActionBinding>
          <ns1:OtherPartyActionBinding>DIGIPOORT_R_Aanleveren</ns1:OtherPartyActionBinding>
        </ns1:CanSend>
        <ns1:CanReceive>
          <ns1:ThisPartyActionBinding ns1:id="OVERHEID_R_BevestigAanleveren" ns1:action="bevestigAanleveren" ns1:packageId="XMLMessagePackage">
            <ns1:BusinessTransactionCharacteristics ns1:isNonRepudiationRequired="true" ns1:isNonRepudiationReceiptRequired="true" ns1:isConfidential="transient" ns1:isAuthenticated="transient-and-persistent" ns1:isTamperProof="transient-and-persistent" ns1:isAuthorizationRequired="true" ns1:isIntelligibleCheckRequired="false" ns1:timeToPerform="P0Y0M2DT0H0M0.000S"/>
            <ns1:ChannelId>OVERHEID_defaultDeliveryChannel_ProfileReliableMessagingSigned</ns1:ChannelId>
          </ns1:ThisPartyActionBinding>
          <ns1:OtherPartyActionBinding>DIGIPOORT_S_BevestigAanleveren</ns1:OtherPartyActionBinding>
        </ns1:CanReceive>
      </ns1:ServiceBinding>
    </ns1:CollaborationRole>
    <ns1:Certificate ns1:certId="OVERHEID_SigningCert">
      <ns3:KeyInfo>
        <ns3:KeyValue>
          <ns3:RSAKeyValue>
            <ns3:Modulus>jOtmODIw7Qv3QOVMyIUbpgT0HDrHBynE2LfjAZk7bNfZrOtNdAq65mIDqUEn8SRsY2zBEmPHxgf38nmJcpcOIORApFDxZMZd44rbrghq+AutKr36krDvLl4kFiUFRefl7IjhyKFwZvH2bK3odwDNIExy285amVduCkG7bPkBcTwOVJbAYR9A2zXJHDZti2fvWMZpEm3B0M7LPJ6KfwCBmPDWXIiqqXIAeOWZjAC4UuqUm6jzkcs9tfO2Qm9bvJZdceeJcWMC90AouL5f8iR3aRt4rHQQZNknmNxTrpqE4G4eTBau9ix9F3iyV15QWgYQLZR7Q2kfZwCV7Zp4JgLYcQ==</ns3:Modulus>
            <ns3:Exponent>AQAB</ns3:Exponent>
          </ns3:RSAKeyValue>
        </ns3:KeyValue>
        <ns3:X509Data>
          <ns3:X509SubjectName>CN=localhost, OU=Rules Matter, O=Ordina, L=Groningen, ST=Groningen, C=NL</ns3:X509SubjectName>
          <ns3:X509IssuerSerial>
            <ns3:X509IssuerName>CN=localhost, OU=Rules Matter, O=Ordina, L=Groningen, ST=Groningen, C=NL</ns3:X509IssuerName>
            <ns3:X509SerialNumber>1336114976</ns3:X509SerialNumber>
          </ns3:X509IssuerSerial>
          <ns3:X509Certificate>MIIDlzCCAn+gAwIBAgIET6N/IDANBgkqhkiG9w0BAQsFADBxMQswCQYDVQQGEwJOTDESMBAGA1UECBMJR3JvbmluZ2VuMRIwEAYDVQQHEwlHcm9uaW5nZW4xDzANBgNVBAoTBk9yZGluYTEVMBMGA1UECxMMUnVsZXMgTWF0dGVyMRIwEAYDVQQDEwlsb2NhbGhvc3QwHhcNMjAwNjA3MTQyOTU5WhcNMzAwNjA1MTQyOTU5WjBxMQswCQYDVQQGEwJOTDESMBAGA1UECBMJR3JvbmluZ2VuMRIwEAYDVQQHEwlHcm9uaW5nZW4xDzANBgNVBAoTBk9yZGluYTEVMBMGA1UECxMMUnVsZXMgTWF0dGVyMRIwEAYDVQQDEwlsb2NhbGhvc3QwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCM62Y4MjDtC/dA5UzIhRumBPQcOscHKcTYt+MBmTts19ms6010CrrmYgOpQSfxJGxjbMESY8fGB/fyeYlylw4g5ECkUPFkxl3jituuCGr4C60qvfqSsO8uXiQWJQVF5+XsiOHIoXBm8fZsreh3AM0gTHLbzlqZV24KQbts+QFxPA5UlsBhH0DbNckcNm2LZ+9YxmkSbcHQzss8nop/AIGY8NZciKqpcgB45ZmMALhS6pSbqPORyz2187ZCb1u8ll1x54lxYwL3QCi4vl/yJHdpG3isdBBk2SeY3FOumoTgbh5MFq72LH0XeLJXXlBaBhAtlHtDaR9nAJXtmngmAthxAgMBAAGjNzA1MBQGA1UdEQQNMAuCCWxvY2FsaG9zdDAdBgNVHQ4EFgQUIGrhAcn6OsrmULDTpbcHhS31ezcwDQYJKoZIhvcNAQELBQADggEBABsqfBJfepASqhu7m7HU97k4aCYiElrJEcyhUluUmAfV/sKo2zD1EtziE4zdQMBHICOYkBLIIvNEpIrYXhsTWOymwjIp+niz8cDYw8Ya57RPNs4cQHZXEnOqwpqal2n9WGIq7Op4OkRT8thQisua/x2zI+N1WzM+MOu5Z/mKRRKQDdWTvNnWpHNp5JZ1TAGbZxHoUUQDQNwEXCMOuzXdE6x8rVLwfEMwpievFnIn9DwegzXM4GBBe+fHErox1WA+ijrcNjl4zhFQ1himdCuEY6AQeAEd5wAalQJ34FpZPfcWRHXRSkUszoiIGArhX0fmL3GeZq+a4tN7XCBNaTPqib4=</ns3:X509Certificate>
        </ns3:X509Data>
      </ns3:KeyInfo>
    </ns1:Certificate>
    <ns1:Certificate ns1:certId="OVERHEID_EncryptionCert">
      <ns3:KeyInfo>
        <ns3:KeyValue>
          <ns3:RSAKeyValue>
            <ns3:Modulus>jOtmODIw7Qv3QOVMyIUbpgT0HDrHBynE2LfjAZk7bNfZrOtNdAq65mIDqUEn8SRsY2zBEmPHxgf38nmJcpcOIORApFDxZMZd44rbrghq+AutKr36krDvLl4kFiUFRefl7IjhyKFwZvH2bK3odwDNIExy285amVduCkG7bPkBcTwOVJbAYR9A2zXJHDZti2fvWMZpEm3B0M7LPJ6KfwCBmPDWXIiqqXIAeOWZjAC4UuqUm6jzkcs9tfO2Qm9bvJZdceeJcWMC90AouL5f8iR3aRt4rHQQZNknmNxTrpqE4G4eTBau9ix9F3iyV15QWgYQLZR7Q2kfZwCV7Zp4JgLYcQ==</ns3:Modulus>
            <ns3:Exponent>AQAB</ns3:Exponent>
          </ns3:RSAKeyValue>
        </ns3:KeyValue>
        <ns3:X509Data>
          <ns3:X509SubjectName>CN=localhost, OU=Rules Matter, O=Ordina, L=Groningen, ST=Groningen, C=NL</ns3:X509SubjectName>
          <ns3:X509IssuerSerial>
            <ns3:X509IssuerName>CN=localhost, OU=Rules Matter, O=Ordina, L=Groningen, ST=Groningen, C=NL</ns3:X509IssuerName>
            <ns3:X509SerialNumber>1336114976</ns3:X509SerialNumber>
          </ns3:X509IssuerSerial>
          <ns3:X509Certificate>MIIDlzCCAn+gAwIBAgIET6N/IDANBgkqhkiG9w0BAQsFADBxMQswCQYDVQQGEwJOTDESMBAGA1UECBMJR3JvbmluZ2VuMRIwEAYDVQQHEwlHcm9uaW5nZW4xDzANBgNVBAoTBk9yZGluYTEVMBMGA1UECxMMUnVsZXMgTWF0dGVyMRIwEAYDVQQDEwlsb2NhbGhvc3QwHhcNMjAwNjA3MTQyOTU5WhcNMzAwNjA1MTQyOTU5WjBxMQswCQYDVQQGEwJOTDESMBAGA1UECBMJR3JvbmluZ2VuMRIwEAYDVQQHEwlHcm9uaW5nZW4xDzANBgNVBAoTBk9yZGluYTEVMBMGA1UECxMMUnVsZXMgTWF0dGVyMRIwEAYDVQQDEwlsb2NhbGhvc3QwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCM62Y4MjDtC/dA5UzIhRumBPQcOscHKcTYt+MBmTts19ms6010CrrmYgOpQSfxJGxjbMESY8fGB/fyeYlylw4g5ECkUPFkxl3jituuCGr4C60qvfqSsO8uXiQWJQVF5+XsiOHIoXBm8fZsreh3AM0gTHLbzlqZV24KQbts+QFxPA5UlsBhH0DbNckcNm2LZ+9YxmkSbcHQzss8nop/AIGY8NZciKqpcgB45ZmMALhS6pSbqPORyz2187ZCb1u8ll1x54lxYwL3QCi4vl/yJHdpG3isdBBk2SeY3FOumoTgbh5MFq72LH0XeLJXXlBaBhAtlHtDaR9nAJXtmngmAthxAgMBAAGjNzA1MBQGA1UdEQQNMAuCCWxvY2FsaG9zdDAdBgNVHQ4EFgQUIGrhAcn6OsrmULDTpbcHhS31ezcwDQYJKoZIhvcNAQELBQADggEBABsqfBJfepASqhu7m7HU97k4aCYiElrJEcyhUluUmAfV/sKo2zD1EtziE4zdQMBHICOYkBLIIvNEpIrYXhsTWOymwjIp+niz8cDYw8Ya57RPNs4cQHZXEnOqwpqal2n9WGIq7Op4OkRT8thQisua/x2zI+N1WzM+MOu5Z/mKRRKQDdWTvNnWpHNp5JZ1TAGbZxHoUUQDQNwEXCMOuzXdE6x8rVLwfEMwpievFnIn9DwegzXM4GBBe+fHErox1WA+ijrcNjl4zhFQ1himdCuEY6AQeAEd5wAalQJ34FpZPfcWRHXRSkUszoiIGArhX0fmL3GeZq+a4tN7XCBNaTPqib4=</ns3:X509Certificate>
        </ns3:X509Data>
      </ns3:KeyInfo>
    </ns1:Certificate>
    <ns1:Certificate ns1:certId="OVERHEID_ServerCert">
      <ns3:KeyInfo>
        <ns3:KeyValue>
          <ns3:RSAKeyValue>
            <ns3:Modulus>jOtmODIw7Qv3QOVMyIUbpgT0HDrHBynE2LfjAZk7bNfZrOtNdAq65mIDqUEn8SRsY2zBEmPHxgf38nmJcpcOIORApFDxZMZd44rbrghq+AutKr36krDvLl4kFiUFRefl7IjhyKFwZvH2bK3odwDNIExy285amVduCkG7bPkBcTwOVJbAYR9A2zXJHDZti2fvWMZpEm3B0M7LPJ6KfwCBmPDWXIiqqXIAeOWZjAC4UuqUm6jzkcs9tfO2Qm9bvJZdceeJcWMC90AouL5f8iR3aRt4rHQQZNknmNxTrpqE4G4eTBau9ix9F3iyV15QWgYQLZR7Q2kfZwCV7Zp4JgLYcQ==</ns3:Modulus>
            <ns3:Exponent>AQAB</ns3:Exponent>
          </ns3:RSAKeyValue>
        </ns3:KeyValue>
        <ns3:X509Data>
          <ns3:X509SubjectName>CN=localhost, OU=Rules Matter, O=Ordina, L=Groningen, ST=Groningen, C=NL</ns3:X509SubjectName>
          <ns3:X509IssuerSerial>
            <ns3:X509IssuerName>CN=localhost, OU=Rules Matter, O=Ordina, L=Groningen, ST=Groningen, C=NL</ns3:X509IssuerName>
            <ns3:X509SerialNumber>1336114976</ns3:X509SerialNumber>
          </ns3:X509IssuerSerial>
          <ns3:X509Certificate>MIIDlzCCAn+gAwIBAgIET6N/IDANBgkqhkiG9w0BAQsFADBxMQswCQYDVQQGEwJOTDESMBAGA1UECBMJR3JvbmluZ2VuMRIwEAYDVQQHEwlHcm9uaW5nZW4xDzANBgNVBAoTBk9yZGluYTEVMBMGA1UECxMMUnVsZXMgTWF0dGVyMRIwEAYDVQQDEwlsb2NhbGhvc3QwHhcNMjAwNjA3MTQyOTU5WhcNMzAwNjA1MTQyOTU5WjBxMQswCQYDVQQGEwJOTDESMBAGA1UECBMJR3JvbmluZ2VuMRIwEAYDVQQHEwlHcm9uaW5nZW4xDzANBgNVBAoTBk9yZGluYTEVMBMGA1UECxMMUnVsZXMgTWF0dGVyMRIwEAYDVQQDEwlsb2NhbGhvc3QwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCM62Y4MjDtC/dA5UzIhRumBPQcOscHKcTYt+MBmTts19ms6010CrrmYgOpQSfxJGxjbMESY8fGB/fyeYlylw4g5ECkUPFkxl3jituuCGr4C60qvfqSsO8uXiQWJQVF5+XsiOHIoXBm8fZsreh3AM0gTHLbzlqZV24KQbts+QFxPA5UlsBhH0DbNckcNm2LZ+9YxmkSbcHQzss8nop/AIGY8NZciKqpcgB45ZmMALhS6pSbqPORyz2187ZCb1u8ll1x54lxYwL3QCi4vl/yJHdpG3isdBBk2SeY3FOumoTgbh5MFq72LH0XeLJXXlBaBhAtlHtDaR9nAJXtmngmAthxAgMBAAGjNzA1MBQGA1UdEQQNMAuCCWxvY2FsaG9zdDAdBgNVHQ4EFgQUIGrhAcn6OsrmULDTpbcHhS31ezcwDQYJKoZIhvcNAQELBQADggEBABsqfBJfepASqhu7m7HU97k4aCYiElrJEcyhUluUmAfV/sKo2zD1EtziE4zdQMBHICOYkBLIIvNEpIrYXhsTWOymwjIp+niz8cDYw8Ya57RPNs4cQHZXEnOqwpqal2n9WGIq7Op4OkRT8thQisua/x2zI+N1WzM+MOu5Z/mKRRKQDdWTvNnWpHNp5JZ1TAGbZxHoUUQDQNwEXCMOuzXdE6x8rVLwfEMwpievFnIn9DwegzXM4GBBe+fHErox1WA+ijrcNjl4zhFQ1himdCuEY6AQeAEd5wAalQJ34FpZPfcWRHXRSkUszoiIGArhX0fmL3GeZq+a4tN7XCBNaTPqib4=</ns3:X509Certificate>
        </ns3:X509Data>
      </ns3:KeyInfo>
    </ns1:Certificate>
    <ns1:Certificate ns1:certId="OVERHEID_ClientCert">
      <ns3:KeyInfo>
        <ns3:KeyValue>
          <ns3:RSAKeyValue>
            <ns3:Modulus>jOtmODIw7Qv3QOVMyIUbpgT0HDrHBynE2LfjAZk7bNfZrOtNdAq65mIDqUEn8SRsY2zBEmPHxgf38nmJcpcOIORApFDxZMZd44rbrghq+AutKr36krDvLl4kFiUFRefl7IjhyKFwZvH2bK3odwDNIExy285amVduCkG7bPkBcTwOVJbAYR9A2zXJHDZti2fvWMZpEm3B0M7LPJ6KfwCBmPDWXIiqqXIAeOWZjAC4UuqUm6jzkcs9tfO2Qm9bvJZdceeJcWMC90AouL5f8iR3aRt4rHQQZNknmNxTrpqE4G4eTBau9ix9F3iyV15QWgYQLZR7Q2kfZwCV7Zp4JgLYcQ==</ns3:Modulus>
            <ns3:Exponent>AQAB</ns3:Exponent>
          </ns3:RSAKeyValue>
        </ns3:KeyValue>
        <ns3:X509Data>
          <ns3:X509SubjectName>CN=localhost, OU=Rules Matter, O=Ordina, L=Groningen, ST=Groningen, C=NL</ns3:X509SubjectName>
          <ns3:X509IssuerSerial>
            <ns3:X509IssuerName>CN=localhost, OU=Rules Matter, O=Ordina, L=Groningen, ST=Groningen, C=NL</ns3:X509IssuerName>
            <ns3:X509SerialNumber>1336114976</ns3:X509SerialNumber>
          </ns3:X509IssuerSerial>
          <ns3:X509Certificate>MIIDlzCCAn+gAwIBAgIET6N/IDANBgkqhkiG9w0BAQsFADBxMQswCQYDVQQGEwJOTDESMBAGA1UECBMJR3JvbmluZ2VuMRIwEAYDVQQHEwlHcm9uaW5nZW4xDzANBgNVBAoTBk9yZGluYTEVMBMGA1UECxMMUnVsZXMgTWF0dGVyMRIwEAYDVQQDEwlsb2NhbGhvc3QwHhcNMjAwNjA3MTQyOTU5WhcNMzAwNjA1MTQyOTU5WjBxMQswCQYDVQQGEwJOTDESMBAGA1UECBMJR3JvbmluZ2VuMRIwEAYDVQQHEwlHcm9uaW5nZW4xDzANBgNVBAoTBk9yZGluYTEVMBMGA1UECxMMUnVsZXMgTWF0dGVyMRIwEAYDVQQDEwlsb2NhbGhvc3QwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCM62Y4MjDtC/dA5UzIhRumBPQcOscHKcTYt+MBmTts19ms6010CrrmYgOpQSfxJGxjbMESY8fGB/fyeYlylw4g5ECkUPFkxl3jituuCGr4C60qvfqSsO8uXiQWJQVF5+XsiOHIoXBm8fZsreh3AM0gTHLbzlqZV24KQbts+QFxPA5UlsBhH0DbNckcNm2LZ+9YxmkSbcHQzss8nop/AIGY8NZciKqpcgB45ZmMALhS6pSbqPORyz2187ZCb1u8ll1x54lxYwL3QCi4vl/yJHdpG3isdBBk2SeY3FOumoTgbh5MFq72LH0XeLJXXlBaBhAtlHtDaR9nAJXtmngmAthxAgMBAAGjNzA1MBQGA1UdEQQNMAuCCWxvY2FsaG9zdDAdBgNVHQ4EFgQUIGrhAcn6OsrmULDTpbcHhS31ezcwDQYJKoZIhvcNAQELBQADggEBABsqfBJfepASqhu7m7HU97k4aCYiElrJEcyhUluUmAfV/sKo2zD1EtziE4zdQMBHICOYkBLIIvNEpIrYXhsTWOymwjIp+niz8cDYw8Ya57RPNs4cQHZXEnOqwpqal2n9WGIq7Op4OkRT8thQisua/x2zI+N1WzM+MOu5Z/mKRRKQDdWTvNnWpHNp5JZ1TAGbZxHoUUQDQNwEXCMOuzXdE6x8rVLwfEMwpievFnIn9DwegzXM4GBBe+fHErox1WA+ijrcNjl4zhFQ1himdCuEY6AQeAEd5wAalQJ34FpZPfcWRHXRSkUszoiIGArhX0fmL3GeZq+a4tN7XCBNaTPqib4=</ns3:X509Certificate>
        </ns3:X509Data>
      </ns3:KeyInfo>
    </ns1:Certificate>
    <ns1:SecurityDetails ns1:securityId="OVERHEID_Security">
      <ns1:TrustAnchors>
        <ns1:AnchorCertificateRef ns1:certId="OVERHEID_SigningCert"/>
        <ns1:AnchorCertificateRef ns1:certId="OVERHEID_EncryptionCert"/>
      </ns1:TrustAnchors>
    </ns1:SecurityDetails>
    <ns1:SecurityDetails ns1:securityId="OVERHEID_TransportSecurity">
      <ns1:TrustAnchors>
        <ns1:AnchorCertificateRef ns1:certId="OVERHEID_ServerCert"/>
        <ns1:AnchorCertificateRef ns1:certId="OVERHEID_ClientCert"/>
      </ns1:TrustAnchors>
    </ns1:SecurityDetails>
    <ns1:DeliveryChannel ns1:channelId="OVERHEID_defaultDeliveryChannel_ProfileBestEffortSigned" ns1:transportId="OVERHEID_transport_HTTPS" ns1:docExchangeId="OVERHEID_SIGNED_BestEffort">
      <ns1:MessagingCharacteristics ns1:syncReplyMode="none" ns1:ackRequested="never" ns1:ackSignatureRequested="never" ns1:duplicateElimination="never" ns1:actor="urn:oasis:names:tc:ebxml-msg:actor:toPartyMSH"/>
    </ns1:DeliveryChannel>
    <ns1:DeliveryChannel ns1:channelId="OVERHEID_defaultDeliveryChannel_ProfileReliableMessagingSigned" ns1:transportId="OVERHEID_transport_HTTPS" ns1:docExchangeId="OVERHEID_SIGNED_ReliableMessaging">
      <ns1:MessagingCharacteristics ns1:syncReplyMode="none" ns1:ackRequested="always" ns1:ackSignatureRequested="never" ns1:duplicateElimination="always" ns1:actor="urn:oasis:names:tc:ebxml-msg:actor:toPartyMSH"/>
    </ns1:DeliveryChannel>
    <ns1:Transport ns1:transportId="OVERHEID_transport_HTTPS">
      <ns1:TransportSender>
        <ns1:TransportProtocol ns1:version="1.1">HTTP</ns1:TransportProtocol>
        <ns1:TransportClientSecurity>
          <ns1:TransportSecurityProtocol ns1:version="3.0">SSL</ns1:TransportSecurityProtocol>
          <ns1:ClientCertificateRef ns1:certId="OVERHEID_ClientCert"/>
          <ns1:ServerSecurityDetailsRef ns1:securityId="OVERHEID_TransportSecurity"/>
        </ns1:TransportClientSecurity>
      </ns1:TransportSender>
      <ns1:TransportReceiver>
        <ns1:TransportProtocol ns1:version="1.1">HTTP</ns1:TransportProtocol>
        <ns1:Endpoint ns1:uri="https://localhost:8088/ebms" ns1:type="allPurpose"/>
        <ns1:TransportServerSecurity>
          <ns1:TransportSecurityProtocol ns1:version="3.0">SSL</ns1:TransportSecurityProtocol>
          <ns1:ServerCertificateRef ns1:certId="OVERHEID_ServerCert"/>
          <ns1:ClientSecurityDetailsRef ns1:securityId="OVERHEID_TransportSecurity"/>
        </ns1:TransportServerSecurity>
      </ns1:TransportReceiver>
    </ns1:Transport>
    <ns1:DocExchange ns1:docExchangeId="OVERHEID_SIGNED_BestEffort">
      <ns1:ebXMLSenderBinding ns1:version="2.0">
        <ns1:SenderNonRepudiation>
          <ns1:NonRepudiationProtocol>http://www.w3.org/2000/09/xmldsig#</ns1:NonRepudiationProtocol>
          <ns1:HashFunction>http://www.w3.org/2000/09/xmldsig#sha1</ns1:HashFunction>
          <ns1:SignatureAlgorithm>http://www.w3.org/2000/09/xmldsig#rsa-sha1</ns1:SignatureAlgorithm>
          <ns1:SigningCertificateRef ns1:certId="OVERHEID_SigningCert"/>
        </ns1:SenderNonRepudiation>
      </ns1:ebXMLSenderBinding>
      <ns1:ebXMLReceiverBinding ns1:version="2.0">
        <ns1:ReceiverNonRepudiation>
          <ns1:NonRepudiationProtocol>http://www.w3.org/2000/09/xmldsig#</ns1:NonRepudiationProtocol>
          <ns1:HashFunction>http://www.w3.org/2000/09/xmldsig#sha1</ns1:HashFunction>
          <ns1:SignatureAlgorithm>http://www.w3.org/2000/09/xmldsig#rsa-sha1</ns1:SignatureAlgorithm>
          <ns1:SigningSecurityDetailsRef ns1:securityId="OVERHEID_Security"/>
        </ns1:ReceiverNonRepudiation>
      </ns1:ebXMLReceiverBinding>
    </ns1:DocExchange>
    <ns1:DocExchange ns1:docExchangeId="OVERHEID_SIGNED_ReliableMessaging">
      <ns1:ebXMLSenderBinding ns1:version="2.0">
        <ns1:ReliableMessaging>
          <ns1:Retries>5</ns1:Retries>
          <ns1:RetryInterval>P0Y0M0DT0H5M0.000S</ns1:RetryInterval>
          <ns1:MessageOrderSemantics>NotGuaranteed</ns1:MessageOrderSemantics>
        </ns1:ReliableMessaging>
        <ns1:PersistDuration>P0Y0M0DT0H30M0.000S</ns1:PersistDuration>
        <ns1:SenderNonRepudiation>
          <ns1:NonRepudiationProtocol>http://www.w3.org/2000/09/xmldsig#</ns1:NonRepudiationProtocol>
          <ns1:HashFunction>http://www.w3.org/2000/09/xmldsig#sha1</ns1:HashFunction>
          <ns1:SignatureAlgorithm>http://www.w3.org/2000/09/xmldsig#rsa-sha1</ns1:SignatureAlgorithm>
          <ns1:SigningCertificateRef ns1:certId="OVERHEID_SigningCert"/>
        </ns1:SenderNonRepudiation>
      </ns1:ebXMLSenderBinding>
      <ns1:ebXMLReceiverBinding ns1:version="2.0">
        <ns1:ReliableMessaging>
          <ns1:Retries>5</ns1:Retries>
          <ns1:RetryInterval>P0Y0M0DT0H5M0.000S</ns1:RetryInterval>
          <ns1:MessageOrderSemantics>NotGuaranteed</ns1:MessageOrderSemantics>
        </ns1:ReliableMessaging>
        <ns1:PersistDuration>P0Y0M0DT0H30M0.000S</ns1:PersistDuration>
        <ns1:ReceiverNonRepudiation>
          <ns1:NonRepudiationProtocol>http://www.w3.org/2000/09/xmldsig#</ns1:NonRepudiationProtocol>
          <ns1:HashFunction>http://www.w3.org/2000/09/xmldsig#sha1</ns1:HashFunction>
          <ns1:SignatureAlgorithm>http://www.w3.org/2000/09/xmldsig#rsa-sha1</ns1:SignatureAlgorithm>
          <ns1:SigningSecurityDetailsRef ns1:securityId="OVERHEID_Security"/>
        </ns1:ReceiverNonRepudiation>
      </ns1:ebXMLReceiverBinding>
    </ns1:DocExchange>
  </ns1:PartyInfo>
  <ns1:SimplePart ns1:id="MsgHdr" ns1:mimetype="text/xml">
    <ns1:NamespaceSupported ns1:location="http://www.oasis-open.org/committees/ebxml-msg/schema/msg-header-2_0.xsd" ns1:version="2.0">http://www.oasis-open.org/committees/ebxml-msg/schema/msg-header-2_0.xsd</ns1:NamespaceSupported>
  </ns1:SimplePart>
  <ns1:SimplePart ns1:id="XMLMsg" ns1:mimetype="application/xml"/>
  <ns1:Packaging ns1:id="MshSignalPackage">
    <ns1:ProcessingCapabilities ns1:parse="true" ns1:generate="true"/>
    <ns1:CompositeList>
      <ns1:Composite ns1:id="MshSignal" ns1:mimetype="multipart/related" ns1:mimeparameters="type=text/xml">
        <ns1:Constituent ns1:idref="MsgHdr"/>
      </ns1:Composite>
    </ns1:CompositeList>
  </ns1:Packaging>
  <ns1:Packaging ns1:id="XMLMessagePackage">
    <ns1:ProcessingCapabilities ns1:parse="true" ns1:generate="true"/>
    <ns1:CompositeList>
      <ns1:Composite ns1:id="Message" ns1:mimetype="multipart/related" ns1:mimeparameters="type=text/xml">
        <ns1:Constituent ns1:idref="MsgHdr"/>
        <ns1:Constituent ns1:idref="XMLMsg"/>
      </ns1:Composite>
    </ns1:CompositeList>
  </ns1:Packaging>
</ns1:CollaborationProtocolAgreement>
"""