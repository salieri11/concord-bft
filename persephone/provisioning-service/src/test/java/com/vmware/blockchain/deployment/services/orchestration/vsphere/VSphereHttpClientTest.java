/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vsphere;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.delete;
import static com.github.tomakehurst.wiremock.client.WireMock.equalTo;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.patch;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathEqualTo;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.AbstractMap;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.runner.RunWith;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.util.UriComponentsBuilder;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.vmware.blockchain.deployment.services.exception.ErrorCode;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.VirtualMachinePowerState;
import com.vmware.blockchain.deployment.services.orchestration.vm.CloudInitConfiguration;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;

@ExtendWith(SpringExtension.class)
@RunWith(PowerMockRunner.class)
@PrepareForTest(VSphereHttpClient.class)
class VSphereHttpClientTest {

    VSphereHttpClient vSphereHttpClient;

    VSphereHttpClient.Context context;

    private static WireMockServer server;

    private static URI vsphereUri;

    private final String goodAuthReturn = "{\n"
                                         + "    \"value\": \"secret string\"\n"
                                         + "}";

    private final String badResponse = "{\n"
            + "    \"value\": [\n"
            + "    ]\n"
            + "}";

    VsphereSessionAuthenticationInterceptor vsphereSessionAuthenticationInterceptor;

    @BeforeEach
    void init() throws Exception {
        server = new WireMockServer(options().dynamicPort());
        server.start();
        // default stub for bad authorization
        server.stubFor(post(urlEqualTo(VsphereEndpoints.VSPHERE_AUTHENTICATION.getPath()))
                               .willReturn(aResponse().withHeader("Content-Type", "application/json")
                                                   .withStatus(401)));

        // stub out the authentication part
        server.stubFor(post(urlEqualTo(VsphereEndpoints.VSPHERE_AUTHENTICATION.getPath()))
                               .withBasicAuth("user", "pass")
                               .willReturn(aResponse().withHeader("Content-Type", "application/json")
                                                   .withBody(goodAuthReturn).withStatus(200)));

        vsphereUri =
                UriComponentsBuilder.newInstance().host("localhost").port(server.port()).scheme("http").build().toUri();
        context = new VSphereHttpClient.Context(vsphereUri, "user", "pass", "");
        vSphereHttpClient = new VSphereHttpClient(context);

    }

    @Test
    void badAuth() throws Exception {
        VSphereHttpClient client =
                new VSphereHttpClient(new VSphereHttpClient.Context(vsphereUri, "no", "body", ""));
    }

    @Test
    void testSelfSignedCertForVSphere() {
        String testCertificateData = "-----BEGIN CERTIFICATE-----\n"
                                     + "MIIDhDCCAmwCCQCqJ2ReGXJGSTANBgkqhkiG9w0BAQsFADCBgzELMAkGA1UEBhMC\n"
                                     + "VVMxCzAJBgNVBAgMAkNBMRIwEAYDVQQHDAlTdW5ueXZhbGUxDzANBgNVBAoMBlZN\n"
                                     + "d2FyZTENMAsGA1UECwwET0NUTzETMBEGA1UEAwwKdm13YXJlLmNvbTEeMBwGCSqG\n"
                                     + "SIb3DQEJARYPdGVzdEB2bXdhcmUuY29tMB4XDTIwMDgyNDIyMDI0M1oXDTIxMDgy\n"
                                     + "NDIyMDI0M1owgYMxCzAJBgNVBAYTAlVTMQswCQYDVQQIDAJDQTESMBAGA1UEBwwJ\n"
                                     + "U3Vubnl2YWxlMQ8wDQYDVQQKDAZWTXdhcmUxDTALBgNVBAsMBE9DVE8xEzARBgNV\n"
                                     + "BAMMCnZtd2FyZS5jb20xHjAcBgkqhkiG9w0BCQEWD3Rlc3RAdm13YXJlLmNvbTCC\n"
                                     + "ASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAKQ53M+a9Rl2n6uR6+Gl2ErT\n"
                                     + "MMwXRixfcdQkBaSfnENf4rSLCo/9nbUFzSDI2N7USY1FlmzHeyvopRNmuyCda6Jv\n"
                                     + "oleleiaHhyrR0FMAUZ8Vz0sI4fMRaRqKBsMJ+QgX4USdmghAkmys7ig5MUJjcU8D\n"
                                     + "UoQ4LoUDiGARkw0oD6cNWa3pdWVfJ3mvaHuq1OlZfQ3kC1luyklhihPIMGrisuua\n"
                                     + "49tGBZs3F6n3Ky3hU09I3okKkBtioXTkYz3Bdszt/XMS5HeyrX/nG2NO60RT3OVX\n"
                                     + "nvdb0bmPSJNmnvSGEzTD8WnA+9Vg13e8xLLA1W6+oFhG6rUD5g5IKnfl6zE8sL0C\n"
                                     + "AwEAATANBgkqhkiG9w0BAQsFAAOCAQEANb0u4elmBugWqAR9reQRlk66Nx3Velab\n"
                                     + "NSI8f78WGHMCS4ryG8fwFKwcJ9XlDJZA81FcRDrycvk0qaLgnSWnBrrauDDkGRL4\n"
                                     + "mJsFnjBIfJkJXDvfp6LYhgubleVj5kiNHCp/Pp9wFnuP7/Q8fgygZrXSiT/tzAQy\n"
                                     + "Au90vbEzxvpCvbf0lQYcL5M9jFt9D2RxMLWQCbf+PrluXe+leWOiPlrvXY/sYqxW\n"
                                     + "hlkQfaYjauR4qTbqo9VX4q142tbAsGnq8tmXbAlL+NK12HHZvGjOlsyOUjAbD0Vt\n"
                                     + "398Vtqg3kiL/IAe1weda08BdTkA/Dj4DKHZQHs2ndKoDVV5icoIXHw==\n"
                                     + "-----END CERTIFICATE-----";

        context = new VSphereHttpClient.Context(vsphereUri, "user", "pass", testCertificateData);
        // Inherently asserting that, no exceptions are thrown
        VSphereHttpClient client = new VSphereHttpClient(context);
    }

    @Test
    void testInvalidCertForVSphere() {
        String testInvalidCertificateData = "-----BEGIN CERTIFICATE-----\n"
                                     + "NIIDhDCCAmwCCQCqJ2ReGXJGSTANBgkqhkiG9w0BAQsFADCBgzELMAkGA1UEBhMC\n"
                                     + "VVMxCzAJBgNVBAgMAkNBMRIwEAYDVQQHDAlTdW5ueXZhbGUxDzANBgNVBAoMBlZN\n"
                                     + "d2FyZTENMAsGA1UECwwET0NUTzETMBEGA1UEAwwKdm13YXJlLmNvbTEeMBwGCSqG\n"
                                     + "SIb3DQEJARYPdGVzdEB2bXdhcmUuY29tMB4XDTIwMDgyNDIyMDI0M1oXDTIxMDgy\n"
                                     + "NDIyMDI0M1owgYMxCzAJBgNVBAYTAlVTMQswCQYDVQQIDAJDQTESMBAGA1UEBwwJ\n"
                                     + "U3Vubnl2YWxlMQ8wDQYDVQQKDAZWTXdhcmUxDTALBgNVBAsMBE9DVE8xEzARBgNV\n"
                                     + "BAMMCnZtd2FyZS5jb20xHjAcBgkqhkiG9w0BCQEWD3Rlc3RAdm13YXJlLmNvbTCC\n"
                                     + "ASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAKQ53M+a9Rl2n6uR6+Gl2ErT\n"
                                     + "MMwXRixfcdQkBaSfnENf4rSLCo/9nbUFzSDI2N7USY1FlmzHeyvopRNmuyCda6Jv\n"
                                     + "oleleiaHhyrR0FMAUZ8Vz0sI4fMRaRqKBsMJ+QgX4USdmghAkmys7ig5MUJjcU8D\n"
                                     + "UoQ4LoUDiGARkw0oD6cNWa3pdWVfJ3mvaHuq1OlZfQ3kC1luyklhihPIMGrisuua\n"
                                     + "49tGBZs3F6n3Ky3hU09I3okKkBtioXTkYz3Bdszt/XMS5HeyrX/nG2NO60RT3OVX\n"
                                     + "nvdb0bmPSJNmnvSGEzTD8WnA+9Vg13e8xLLA1W6+oFhG6rUD5g5IKnfl6zE8sL0C\n"
                                     + "AwEAATANBgkqhkiG9w0BAQsFAAOCAQEANb0u4elmBugWqAR9reQRlk66Nx3Velab\n"
                                     + "NSI8f78WGHMCS4ryG8fwFKwcJ9XlDJZA81FcRDrycvk0qaLgnSWnBrrauDDkGRL4\n"
                                     + "mJsFnjBIfJkJXDvfp6LYhgubleVj5kiNHCp/Pp9wFnuP7/Q8fgygZrXSiT/tzAQy\n"
                                     + "Au90vbEzxvpCvbf0lQYcL5M9jFt9D2RxMLWQCbf+PrluXe+leWOiPlrvXY/sYqxW\n"
                                     + "hlkQfaYjauR4qTbqo9VX4q142tbAsGnq8tmXbAlL+NK12HHZvGjOlsyOUjAbD0Vt\n"
                                     + "398Vtqg3kiL/IAe1weda08BdTkA/Dj4DKHZQHs2ndKoDVV5icoIXHw==\n"
                                     + "-----END CERTIFICATE-----";

        context = new VSphereHttpClient.Context(vsphereUri, "user", "pass", testInvalidCertificateData);
        // Because of invalid certificate in certificateData
        Assertions.assertThrows(
                PersephoneException.class,
            () -> new VSphereHttpClient(context)
        );
    }

    @Test
    void testMultipleCertsForVSphere() {
        String testMultipleCertificateData = "-----BEGIN CERTIFICATE-----\n"
                                     + "MIIEPjCCAyagAwIBAgIESlOMKDANBgkqhkiG9w0BAQsFADCBvjELMAkGA1UEBhMC\n"
                                     + "VVMxFjAUBgNVBAoTDUVudHJ1c3QsIEluYy4xKDAmBgNVBAsTH1NlZSB3d3cuZW50\n"
                                     + "cnVzdC5uZXQvbGVnYWwtdGVybXMxOTA3BgNVBAsTMChjKSAyMDA5IEVudHJ1c3Qs\n"
                                     + "IEluYy4gLSBmb3IgYXV0aG9yaXplZCB1c2Ugb25seTEyMDAGA1UEAxMpRW50cnVz\n"
                                     + "dCBSb290IENlcnRpZmljYXRpb24gQXV0aG9yaXR5IC0gRzIwHhcNMDkwNzA3MTcy\n"
                                     + "NTU0WhcNMzAxMjA3MTc1NTU0WjCBvjELMAkGA1UEBhMCVVMxFjAUBgNVBAoTDUVu\n"
                                     + "dHJ1c3QsIEluYy4xKDAmBgNVBAsTH1NlZSB3d3cuZW50cnVzdC5uZXQvbGVnYWwt\n"
                                     + "dGVybXMxOTA3BgNVBAsTMChjKSAyMDA5IEVudHJ1c3QsIEluYy4gLSBmb3IgYXV0\n"
                                     + "aG9yaXplZCB1c2Ugb25seTEyMDAGA1UEAxMpRW50cnVzdCBSb290IENlcnRpZmlj\n"
                                     + "YXRpb24gQXV0aG9yaXR5IC0gRzIwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEK\n"
                                     + "AoIBAQC6hLZy254Ma+KZ6TABp3bqMriVQRrJ2mFOWHLP/vaCeb9zYQYKpSfYs1/T\n"
                                     + "RU4cctZOMvJyig/3gxnQaoCAAEUesMfnmr8SVycco2gvCoe9amsOXmXzHHfV1IWN\n"
                                     + "cCG0szLni6LVhjkCsbjSR87kyUnEO6fe+1R9V77w6G7CebI6C1XiUJgWMhNcL3hW\n"
                                     + "wcKUs/Ja5CeanyTXxuzQmyWC48zCxEXFjJd6BmsqEZ+pCm5IO2/b1BEZQvePB7/1\n"
                                     + "U1+cPvQXLOZprE4yTGJ36rfo5bs0vBmLrpxR57d+tVOxMyLlbc9wPBr64ptntoP0\n"
                                     + "jaWvYkxN4FisZDQSA/i2jZRjJKRxAgMBAAGjQjBAMA4GA1UdDwEB/wQEAwIBBjAP\n"
                                     + "BgNVHRMBAf8EBTADAQH/MB0GA1UdDgQWBBRqciZ60B7vfec7aVHUbI2fkBJmqzAN\n"
                                     + "BgkqhkiG9w0BAQsFAAOCAQEAeZ8dlsa2eT8ijYfThwMEYGprmi5ZiXMRrEPR9RP/\n"
                                     + "jTkrwPK9T3CMqS/qF8QLVJ7UG5aYMzyorWKiAHarWWluBh1+xLlEjZivEtRh2woZ\n"
                                     + "Rkfz6/djwUAFQKXSt/S1mja/qYh2iARVBCuch38aNzx+LaUa2NSJXsq9rD1s2G2v\n"
                                     + "1fN2D807iDginWyTmsQ9v4IbZT+mD12q/OWyFcq1rca8PdCE6OoGcrBNOTJ4vz4R\n"
                                     + "nAuknZoh8/CbCzB428Hch0P+vGOaysXCHMnHjf87ElgI5rY97HosTvuDls4MPGmH\n"
                                     + "VHOkc8KT/1EQrBVUAdj8BbGJoX90g5pJ19xOe4pIb4tF9g==\n"
                                     + "-----END CERTIFICATE-----\n"
                                     + "-----BEGIN CERTIFICATE-----\n"
                                     + "MIIFDjCCA/agAwIBAgIMDulMwwAAAABR03eFMA0GCSqGSIb3DQEBCwUAMIG+MQsw\n"
                                     + "CQYDVQQGEwJVUzEWMBQGA1UEChMNRW50cnVzdCwgSW5jLjEoMCYGA1UECxMfU2Vl\n"
                                     + "IHd3dy5lbnRydXN0Lm5ldC9sZWdhbC10ZXJtczE5MDcGA1UECxMwKGMpIDIwMDkg\n"
                                     + "RW50cnVzdCwgSW5jLiAtIGZvciBhdXRob3JpemVkIHVzZSBvbmx5MTIwMAYDVQQD\n"
                                     + "EylFbnRydXN0IFJvb3QgQ2VydGlmaWNhdGlvbiBBdXRob3JpdHkgLSBHMjAeFw0x\n"
                                     + "NTEwMDUxOTEzNTZaFw0zMDEyMDUxOTQzNTZaMIG6MQswCQYDVQQGEwJVUzEWMBQG\n"
                                     + "A1UEChMNRW50cnVzdCwgSW5jLjEoMCYGA1UECxMfU2VlIHd3dy5lbnRydXN0Lm5l\n"
                                     + "dC9sZWdhbC10ZXJtczE5MDcGA1UECxMwKGMpIDIwMTIgRW50cnVzdCwgSW5jLiAt\n"
                                     + "IGZvciBhdXRob3JpemVkIHVzZSBvbmx5MS4wLAYDVQQDEyVFbnRydXN0IENlcnRp\n"
                                     + "ZmljYXRpb24gQXV0aG9yaXR5IC0gTDFLMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8A\n"
                                     + "MIIBCgKCAQEA2j+W0E25L0Tn2zlem1DuXKVh2kFnUwmqAJqOV38pa9vH4SEkqjrQ\n"
                                     + "jUcj0u1yFvCRIdJdt7hLqIOPt5EyaM/OJZMssn2XyP7BtBe6CZ4DkJN7fEmDImiK\n"
                                     + "m95HwzGYei59QAvS7z7Tsoyqj0ip/wDoKVgG97aTWpRzJiatWA7lQrjV6nN5ZGhT\n"
                                     + "JbiEz5R6rgZFDKNrTdDGvuoYpDbwkrK6HIiPOlJ/915tgxyd8B/lw9bdpXiSPbBt\n"
                                     + "LOrJz5RBGXFEaLpHPATpXbo+8DX3Fbae8i4VHj9HyMg4p3NFXU2wO7GOFyk36t0F\n"
                                     + "ASK7lDYqjVs1/lMZLwhGwSqzGmIdTivZGwIDAQABo4IBDDCCAQgwDgYDVR0PAQH/\n"
                                     + "BAQDAgEGMBIGA1UdEwEB/wQIMAYBAf8CAQAwMwYIKwYBBQUHAQEEJzAlMCMGCCsG\n"
                                     + "AQUFBzABhhdodHRwOi8vb2NzcC5lbnRydXN0Lm5ldDAwBgNVHR8EKTAnMCWgI6Ah\n"
                                     + "hh9odHRwOi8vY3JsLmVudHJ1c3QubmV0L2cyY2EuY3JsMDsGA1UdIAQ0MDIwMAYE\n"
                                     + "VR0gADAoMCYGCCsGAQUFBwIBFhpodHRwOi8vd3d3LmVudHJ1c3QubmV0L3JwYTAd\n"
                                     + "BgNVHQ4EFgQUgqJwdN28Uz/Pe9T3zX+nYMYKTL8wHwYDVR0jBBgwFoAUanImetAe\n"
                                     + "733nO2lR1GyNn5ASZqswDQYJKoZIhvcNAQELBQADggEBADnVjpiDYcgsY9NwHRkw\n"
                                     + "y/YJrMxp1cncN0HyMg/vdMNY9ngnCTQIlZIv19+4o/0OgemknNM/TWgrFTEKFcxS\n"
                                     + "BJPok1DD2bHi4Wi3Ogl08TRYCj93mEC45mj/XeTIRsXsgdfJghhcg85x2Ly/rJkC\n"
                                     + "k9uUmITSnKa1/ly78EqvIazCP0kkZ9Yujs+szGQVGHLlbHfTUqi53Y2sAEo1GdRv\n"
                                     + "c6N172tkw+CNgxKhiucOhk3YtCAbvmqljEtoZuMrx1gL+1YQ1JH7HdMxWBCMRON1\n"
                                     + "exCdtTix9qrKgWRs6PLigVWXUX/hwidQosk8WwBD9lu51aX8/wdQQGcHsFXwt35u\n"
                                     + "Lcw=\n-----END CERTIFICATE-----\n"
                                     + "-----BEGIN CERTIFICATE-----\n"
                                     + "MIIEkTCCA3mgAwIBAgIERWtQVDANBgkqhkiG9w0BAQUFADCBsDELMAkGA1UEBhMC\n"
                                     + "VVMxFjAUBgNVBAoTDUVudHJ1c3QsIEluYy4xOTA3BgNVBAsTMHd3dy5lbnRydXN0\n"
                                     + "Lm5ldC9DUFMgaXMgaW5jb3Jwb3JhdGVkIGJ5IHJlZmVyZW5jZTEfMB0GA1UECxMW\n"
                                     + "KGMpIDIwMDYgRW50cnVzdCwgSW5jLjEtMCsGA1UEAxMkRW50cnVzdCBSb290IENl\n"
                                     + "cnRpZmljYXRpb24gQXV0aG9yaXR5MB4XDTA2MTEyNzIwMjM0MloXDTI2MTEyNzIw\n"
                                     + "NTM0MlowgbAxCzAJBgNVBAYTAlVTMRYwFAYDVQQKEw1FbnRydXN0LCBJbmMuMTkw\n"
                                     + "NwYDVQQLEzB3d3cuZW50cnVzdC5uZXQvQ1BTIGlzIGluY29ycG9yYXRlZCBieSBy\n"
                                     + "ZWZlcmVuY2UxHzAdBgNVBAsTFihjKSAyMDA2IEVudHJ1c3QsIEluYy4xLTArBgNV\n"
                                     + "BAMTJEVudHJ1c3QgUm9vdCBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eTCCASIwDQYJ\n"
                                     + "KoZIhvcNAQEBBQADggEPADCCAQoCggEBALaVtkNC+sZtKm9I35RMOVcF7sN5EUFo\n"
                                     + "Nu3s/poBj6E4KPz3EEZmLk0eGrEaTsbRwJWIsMn/MYszA9u3g3s+IIRe7bJWKKf4\n"
                                     + "4LlAcTfFy0cOlypowCKVYhXbR9n10Cv/gkvJrT7eTNuQgFA/CYqEAOwwCj0Yzfv9\n"
                                     + "KlmaI5UXLEWeH25DeW0MXJj+SKfFI0dcXv1u5x609mhF0YaDW6KKjbHjKYD+JXGI\n"
                                     + "rb68j6xSlkuqUY3kEzEZ6E5Nn9uss2rVvDlUccp6en+Q3X0dgNmBu1kmwhH+5pPi\n"
                                     + "94DkZfs0Nw4pgHBNrziGLp5/V6+eF67rHMsoIV+2HNjnogQi+dPa2MsCAwEAAaOB\n"
                                     + "sDCBrTAOBgNVHQ8BAf8EBAMCAQYwDwYDVR0TAQH/BAUwAwEB/zArBgNVHRAEJDAi\n"
                                     + "gA8yMDA2MTEyNzIwMjM0MlqBDzIwMjYxMTI3MjA1MzQyWjAfBgNVHSMEGDAWgBRo\n"
                                     + "kORnpKZTgMeGZqTx90tD+4S9bTAdBgNVHQ4EFgQUaJDkZ6SmU4DHhmak8fdLQ/uE\n"
                                     + "vW0wHQYJKoZIhvZ9B0EABBAwDhsIVjcuMTo0LjADAgSQMA0GCSqGSIb3DQEBBQUA\n"
                                     + "A4IBAQCT1DCw1wMgKtD5Y+iRDAUgqV8ZyntyTtSx29CW+1RaGSwMCPeyvIWonX9t\n"
                                     + "O1KzKtvn1ISMY/YPyyYBkVBs9F8U4pN0wBOeMDpQ47RgxRzwIkSNcUesyBrJ6Zua\n"
                                     + "AGAT/3B+XxFNSRuzFVJ7yVTav52Vr2ua2J7p8eRDjeIRRDq/r72DQnNSi6q7pynP\n"
                                     + "9WQcCk3RvKqsnyrQ/39/2n3qse0wJcGE2jTSW3iDVuycNsMm4hH2Z0kdkquM++v/\n"
                                     + "eu6FSqdQgPCnXEqULl8FmTxSQeDNtGPPAUO6nIPcj2A781q0tHuu2guQOHXvgR1m\n"
                                     + "0vdXcDazv/wor3ElhVsT/h5/WrQ8\n"
                                     + "-----END CERTIFICATE-----\n";

        context = new VSphereHttpClient.Context(vsphereUri, "user", "pass", testMultipleCertificateData);
        // Inherently asserting that, no exceptions are thrown
        VSphereHttpClient client = new VSphereHttpClient(context);
    }

    @Test
    void testIncompleteCertForVsphere() {
        String testCertificateData = "-----BEGIN CERTIFICATE-----\n"
                                     + "MIIEPjCCAyagAwIBAgIESlOMKDANBgkqhkiG9w0BAQsFADCBvjELMAkGA1UEBhMC\n"
                                     + "Rkfz6/djwUAFQKXSt/S1mja/qYh2iARVBCuch38aNzx+LaUa2NSJXsq9rD1s2G2v\n";

        context = new VSphereHttpClient.Context(vsphereUri, "user", "pass", testCertificateData);

        // Because of incomplete data in certificateData
        Assertions.assertThrows(
                PersephoneException.class,
            () -> new VSphereHttpClient(context)
        );
    }

    @Test
    void testIncompleteMultipleCertForVsphere() {
        String testIncompleteCertificateData = "-----BEGIN CERTIFICATE-----\n"
                                     + "MIIEPjCCAyagAwIBAgIESlOMKDANBgkqhkiG9w0BAQsFADCBvjELMAkGA1UEBhMC\n"
                                     + "VVMxFjAUBgNVBAoTDUVudHJ1c3QsIEluYy4xKDAmBgNVBAsTH1NlZSB3d3cuZW50\n"
                                     + "cnVzdC5uZXQvbGVnYWwtdGVybXMxOTA3BgNVBAsTMChjKSAyMDA5IEVudHJ1c3Qs\n"
                                     + "IEluYy4gLSBmb3IgYXV0aG9yaXplZCB1c2Ugb25seTEyMDAGA1UEAxMpRW50cnVz\n"
                                     + "dCBSb290IENlcnRpZmljYXRpb24gQXV0aG9yaXR5IC0gRzIwHhcNMDkwNzA3MTcy\n"
                                     + "NTU0WhcNMzAxMjA3MTc1NTU0WjCBvjELMAkGA1UEBhMCVVMxFjAUBgNVBAoTDUVu\n"
                                     + "dHJ1c3QsIEluYy4xKDAmBgNVBAsTH1NlZSB3d3cuZW50cnVzdC5uZXQvbGVnYWwt\n"
                                     + "dGVybXMxOTA3BgNVBAsTMChjKSAyMDA5IEVudHJ1c3QsIEluYy4gLSBmb3IgYXV0\n"
                                     + "aG9yaXplZCB1c2Ugb25seTEyMDAGA1UEAxMpRW50cnVzdCBSb290IENlcnRpZmlj\n"
                                     + "YXRpb24gQXV0aG9yaXR5IC0gRzIwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEK\n"
                                     + "AoIBAQC6hLZy254Ma+KZ6TABp3bqMriVQRrJ2mFOWHLP/vaCeb9zYQYKpSfYs1/T\n"
                                     + "RU4cctZOMvJyig/3gxnQaoCAAEUesMfnmr8SVycco2gvCoe9amsOXmXzHHfV1IWN\n"
                                     + "cCG0szLni6LVhjkCsbjSR87kyUnEO6fe+1R9V77w6G7CebI6C1XiUJgWMhNcL3hW\n"
                                     + "wcKUs/Ja5CeanyTXxuzQmyWC48zCxEXFjJd6BmsqEZ+pCm5IO2/b1BEZQvePB7/1\n"
                                     + "U1+cPvQXLOZprE4yTGJ36rfo5bs0vBmLrpxR57d+tVOxMyLlbc9wPBr64ptntoP0\n"
                                     + "jaWvYkxN4FisZDQSA/i2jZRjJKRxAgMBAAGjQjBAMA4GA1UdDwEB/wQEAwIBBjAP\n"
                                     + "BgNVHRMBAf8EBTADAQH/MB0GA1UdDgQWBBRqciZ60B7vfec7aVHUbI2fkBJmqzAN\n"
                                     + "BgkqhkiG9w0BAQsFAAOCAQEAeZ8dlsa2eT8ijYfThwMEYGprmi5ZiXMRrEPR9RP/\n"
                                     + "jTkrwPK9T3CMqS/qF8QLVJ7UG5aYMzyorWKiAHarWWluBh1+xLlEjZivEtRh2woZ\n"
                                     + "Rkfz6/djwUAFQKXSt/S1mja/qYh2iARVBCuch38aNzx+LaUa2NSJXsq9rD1s2G2v\n"
                                     + "1fN2D807iDginWyTmsQ9v4IbZT+mD12q/OWyFcq1rca8PdCE6OoGcrBNOTJ4vz4R\n"
                                     + "nAuknZoh8/CbCzB428Hch0P+vGOaysXCHMnHjf87ElgI5rY97HosTvuDls4MPGmH\n"
                                     + "VHOkc8KT/1EQrBVUAdj8BbGJoX90g5pJ19xOe4pIb4tF9g==\n"
                                     + "-----END CERTIFICATE-----\n"
                                     + "-----BEGIN CERTIFICATE-----\n"
                                     + "MIIFDjCCA/agAwIBAgIMDulMwwAAAABR03eFMA0GCSqGSIb3DQEBCwUAMIG+MQsw\n"
                                     + "CQYDVQQGEwJVUzEWMBQGA1UEChMNRW50cnVzdCwgSW5jLjEoMCYGA1UECxMfU2Vl\n";

        context = new VSphereHttpClient.Context(vsphereUri, "user", "pass", testIncompleteCertificateData);

        // Because of incomplete data in the second certificate of certificateData
        Assertions.assertThrows(
                PersephoneException.class,
            () -> new VSphereHttpClient(context)
        );
    }

    @Test
    void testGetGoodFolder() {
        String goodFolderResponseString = "{\n"
                                          + "    \"value\": [\n"
                                          + "        {\n"
                                          + "            \"folder\": \"Offensive Spear Gungnir\",\n"
                                          + "            \"name\": \"Good_folder\",\n"
                                          + "            \"type\": \"VIRTUAL_MACHINE\"\n"
                                          + "        }"
                                          + "    ]\n"
                                          + "}";

        String goodFolder = "Offensive Spear Gungnir";
        String goodFolderName = "Good_folder";

        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_FOLDERS.getPath()))
                               .withQueryParam("filter.type", equalTo("VIRTUAL_MACHINE"))
                               .withQueryParam("filter.names", equalTo(goodFolderName))
                               .willReturn(aResponse()
                                                   .withHeader("Content-Type", "application/json")
                                                   .withBody(goodFolderResponseString).withStatus(200)));

        String folder = vSphereHttpClient.getFolder(goodFolderName);
        Assertions.assertEquals(goodFolder, folder);
    }

    @Test
    void testGetBadFolder() {
        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_FOLDERS.getPath()))
                .willReturn(aResponse().withHeader("Content-Type", "application/json")
                        .withBody(badResponse).withStatus(200)));

        String badFolderName = "Bad_folder";
        Assertions.assertThrows(
                PersephoneException.class,
            () -> vSphereHttpClient.getFolder(badFolderName)
        );
    }

    @Test
    void getGoodResourcePool() {
        String goodResourcePoolResponseString = "{\n"
                + "    \"value\": [\n"
                + "        {\n"
                + "            \"resource_pool\": \"Tiny Adventurer\",\n"
                + "            \"name\": \"Good_resource_pool\"\n"
                + "        }"
                + "    ]\n"
                + "}";

        String goodResourcePool = "Tiny Adventurer";
        String goodResourcePoolName = "Good_resource_pool";

        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_RESOURCE_POOLS.getPath()))
                .withQueryParam("filter.names", equalTo(goodResourcePoolName))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(goodResourcePoolResponseString).withStatus(200)));



        String resourcePool = vSphereHttpClient.getResourcePool(goodResourcePoolName);

        Assertions.assertEquals(goodResourcePool, resourcePool);
    }

    @Test
    void testGetBadResourcePool() {
        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_RESOURCE_POOLS.getPath()))
                .willReturn(aResponse().withHeader("Content-Type", "application/json")
                        .withBody(badResponse).withStatus(200)));

        String badResourcePoolName = "Bad_resource_pool";
        Assertions.assertThrows(
                PersephoneException.class,
            () -> vSphereHttpClient.getResourcePool(badResourcePoolName)
        );
    }

    @Test
    void getGoodDatastore() {
        String goodDatastoreResponseString = "{\n"
                + "    \"value\": [\n"
                + "        {\n"
                + "            \"datastore\": \"Searching for bliss\",\n"
                + "            \"name\": \"Good_datastore\",\n"
                + "            \"type\": \"Good_datastore_type\",\n"
                + "            \"free_space\": 100,\n"
                + "            \"capacity\": 200\n"
                + "        }"
                + "    ]\n"
                + "}";

        String goodDatastore = "Searching for bliss";
        String goodDatastoreName = "Good_datastore";

        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_DATASTORES.getPath()))
                .withQueryParam("filter.names", equalTo(goodDatastoreName))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(goodDatastoreResponseString).withStatus(200)));

        String datastore = vSphereHttpClient.getDatastore(goodDatastoreName);

        Assertions.assertEquals(goodDatastore, datastore);
    }

    @Test
    void getBadDatastore() {
        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_DATASTORES.getPath()))
                .willReturn(aResponse().withHeader("Content-Type", "application/json")
                        .withBody(badResponse).withStatus(200)));

        String badDatastoreName = "Bad_datastore";
        Assertions.assertThrows(
                PersephoneException.class,
            () -> vSphereHttpClient.getDatastore(badDatastoreName)
        );
    }

    @Test
    void getGoodNetwork() {
        String goodNetworkResponseString = "{\n"
                + "    \"value\": [\n"
                + "        {\n"
                + "            \"network\": \"BLOODY STREAM\",\n"
                + "            \"name\": \"Good_network\",\n"
                + "            \"type\": \"Good_network_type\"\n"
                + "        }"
                + "    ]\n"
                + "}";

        String goodNetwork = "BLOODY STREAM";
        String goodNetworkName = "Good_network";

        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_NETWORKS.getPath()))
                .withQueryParam("filter.names", equalTo(goodNetworkName))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(goodNetworkResponseString).withStatus(200)));



        String network = vSphereHttpClient.getNetwork(goodNetworkName);

        Assertions.assertEquals(goodNetwork, network);
    }

    @Test
    void getBadNetwork() {
        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_NETWORKS.getPath()))
                .willReturn(aResponse().withHeader("Content-Type", "application/json")
                        .withBody(badResponse).withStatus(200)));

        String badNetworkName = "Bad_network";
        Assertions.assertThrows(
                PersephoneException.class,
            () -> vSphereHttpClient.getNetwork(badNetworkName)
        );
    }

    @Test
    void getGoodLibraryItem() {
        String goodLibraryItemResponseString = "{\n"
                + "    \"value\": [\"Monochrome Classroom\"]\n"
                + "}";

        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_CONTENT_LIBRARY_ITEM.getPath()))
                .withQueryParam("~action", WireMock.equalTo("find"))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(goodLibraryItemResponseString).withStatus(200)));

        String sourceId = "sourceId";
        String libraryItem = vSphereHttpClient.getLibraryItem(sourceId);

        String goodLibraryItem = "Monochrome Classroom";
        Assertions.assertEquals(goodLibraryItem, libraryItem);
    }

    @Test
    void getBadLibraryItem() {
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_CONTENT_LIBRARY_ITEM.getPath()))
                .withQueryParam("~action", WireMock.equalTo("find"))
                .willReturn(aResponse().withHeader("Content-Type", "application/json")
                        .withBody(badResponse).withStatus(200)));

        String sourceId = "sourceId";
        Assertions.assertThrows(
                PersephoneException.class,
            () -> vSphereHttpClient.getLibraryItem(sourceId)
        );
    }

    @Test
    void createVirtualMachine() {
        String goodVmCreateResponseString = "{\n"
                + "    \"value\": \n"
                + "        {\n"
                + "            \"succeeded\": true,\n"
                + "            \"resource_id\": {\n"
                + "                 \"type\": \"Fire\",\n"
                + "                 \"id\": \"Steins:Gate\""
                + "            }"
                + "        }"
                + "}";

        String libraryItem = "libraryItem";
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_OVF_LIBRARY_ITEM.getPath().replace("{library_item}",
                libraryItem)))
                .withQueryParam("~action", WireMock.equalTo("deploy"))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(goodVmCreateResponseString).withStatus(200)));

        String name = "Inverted Bridge";
        String datastore = "dataStore";
        String resourcePool = "resourcePool";
        String folder = "folder";
        Map.Entry<String, String> networks = new AbstractMap.SimpleEntry<>("frip", "Side");

        CloudInitConfiguration cloudInit = mock(CloudInitConfiguration.class);
        when(cloudInit.userData()).thenReturn("Gibberish for testing purposes");

        String createVirtualMachine = vSphereHttpClient.createVirtualMachine(name, libraryItem, datastore,
                resourcePool, folder, networks, cloudInit);

        Assertions.assertEquals(createVirtualMachine, "Steins:Gate");
    }

    @Test
    void createVirtualMachineWithRetry() {
        String goodVmCreateResponseString = "{\n"
                                            + "    \"value\": \n"
                                            + "        {\n"
                                            + "            \"succeeded\": true,\n"
                                            + "            \"resource_id\": {\n"
                                            + "                 \"type\": \"Fire\",\n"
                                            + "                 \"id\": \"Steins:Gate\""
                                            + "            }"
                                            + "        }"
                                            + "}";

        String emtpyVmCreateResponseString = "{\n"
                                            + "    \"value\": \n"
                                            + "        {\n"
                                            + "            \"succeeded\": true\n"
                                            + "        }"
                                            + "}";

        String libraryItem = "libraryItem";
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_OVF_LIBRARY_ITEM.getPath().replace("{library_item}",
                                                                                                       libraryItem)))
                               .withQueryParam("~action", WireMock.equalTo("deploy"))
                               .willReturn(aResponse()
                                                   .withHeader("Content-Type", "application/json")
                                                   .withBody(emtpyVmCreateResponseString).withStatus(200)));

        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_OVF_LIBRARY_ITEM.getPath().replace("{library_item}",
                                                                                                       libraryItem)))
                               .withQueryParam("~action", WireMock.equalTo("deploy"))
                               .willReturn(aResponse()
                                                   .withHeader("Content-Type", "application/json")
                                                   .withBody(goodVmCreateResponseString).withStatus(200)));

        String name = "Inverted Bridge";
        String datastore = "dataStore";
        String resourcePool = "resourcePool";
        String folder = "folder";
        Map.Entry<String, String> networks = new AbstractMap.SimpleEntry<>("frip", "Side");

        CloudInitConfiguration cloudInit = mock(CloudInitConfiguration.class);
        when(cloudInit.userData()).thenReturn("Gibberish for testing purposes");

        String createVirtualMachine = vSphereHttpClient.createVirtualMachine(name, libraryItem, datastore,
                                                                             resourcePool, folder, networks, cloudInit);

        Assertions.assertEquals(createVirtualMachine, "Steins:Gate");
    }

    @Test
    void createVirtualMachineWithRetryFailure() {

        String emtpyVmCreateResponseString = "{\n"
                                             + "    \"value\": \n"
                                             + "        {\n"
                                             + "            \"succeeded\": true\n"
                                             + "        }"
                                             + "}";

        String libraryItem = "libraryItem";
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_OVF_LIBRARY_ITEM.getPath().replace("{library_item}",
                                                                                                       libraryItem)))
                               .withQueryParam("~action", WireMock.equalTo("deploy"))
                               .willReturn(aResponse()
                                                   .withHeader("Content-Type", "application/json")
                                                   .withBody(emtpyVmCreateResponseString).withStatus(200)));

        String name = "Inverted Bridge";
        String datastore = "dataStore";
        String resourcePool = "resourcePool";
        String folder = "folder";
        Map.Entry<String, String> networks = new AbstractMap.SimpleEntry<>("frip", "Side");

        CloudInitConfiguration cloudInit = mock(CloudInitConfiguration.class);
        when(cloudInit.userData()).thenReturn("Gibberish for testing purposes");

        Assertions.assertThrows(
                PersephoneException.class,
            () -> vSphereHttpClient
                        .createVirtualMachine(name, libraryItem, datastore, resourcePool, folder, networks, cloudInit)
        );
    }

    @Test
    void updateVirtualMachineMemory() {
        String name = "skyv2008";

        server.stubFor(patch(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_MEMORY_UPDATE.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        long memory = 1000;
        Assertions.assertTrue(vSphereHttpClient.updateVirtualMachineMemory(name, memory));
    }

    @Test
    void updateVirtualMachineMemoryBad() {
        String name = "skyv2008";

        server.stubFor(patch(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_MEMORY_UPDATE.getPath().replace("{vm}", name)))
                               .willReturn(aResponse()
                                                   .withHeader("Content-Type", "application/json")
                                                   .withStatus(500)));
        long memory = 1000;
        PersephoneException pe = Assertions.assertThrows(PersephoneException.class,
            () -> vSphereHttpClient.updateVirtualMachineMemory(name, 5)
        );
        Assertions.assertTrue(pe.getCause() instanceof PersephoneException);
        Assertions.assertTrue(pe.getMessage().startsWith(ErrorCode.VM_MEMORY_UPGRADE_ERROR.substring(0, 25)));
    }

    @Test
    void updateVirtualMachineCpu() {
        String name = "Jumpin";
        server.stubFor(patch(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_CPU_UPDATE.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        Assertions.assertTrue(vSphereHttpClient.updateVirtualMachineCpu(name, 10, 10));
    }

    @Test
    void createVirtualMachineDisk() {
        String name = "vm-007";

        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_DISK_CREATE.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        long diskGb = 64;
        Assertions.assertTrue(vSphereHttpClient.createVirtualMachineDisk(name, diskGb));
    }

    @Test
    void createVirtualMachineDiskBad() {
        String name = "vm-008";
        try {
            server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_DISK_CREATE.getPath().replace("{vm}", name)))
                                   .willReturn(aResponse()
                                                       .withHeader("Content-Type", "application/json")
                                                       .withStatus(400)));
        } catch (Exception e) {
            Assertions.assertTrue(e instanceof PersephoneException);
            Assertions.assertTrue(e.getMessage().startsWith(ErrorCode.VM_DISK_CREATE_ERROR));
        }
    }

    @Test
    void deleteExistingVirtualMachine() {
        String id = "Snowstorm";
        server.stubFor(delete(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM.getPath().replace("{vm}", id)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        Assertions.assertTrue(vSphereHttpClient.deleteVirtualMachine(id));
    }

    @Test
    void deleteNotFoundVirtualMachine() {
        String id = "Fleeting";

        server.stubFor(delete(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM.getPath().replace("{vm}", id)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(404)));

        Assertions.assertTrue(vSphereHttpClient.deleteVirtualMachine(id));
    }

    @Test
    void deleteForbiddenVirtualMachine() {
        String id = "Backlit_Wings";

        server.stubFor(delete(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM.getPath().replace("{vm}", id)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(403)));

        Assertions.assertFalse(vSphereHttpClient.deleteVirtualMachine(id));
    }

    @Test
    void powerOnVirtualMachine() {
        String name = "Snowy_Daydream";
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_POWER_START.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        Assertions.assertTrue(vSphereHttpClient.updateVirtualMachinePowerState(name,
                VirtualMachinePowerState.POWERED_ON));
    }

    @Test
    void powerOffVirtualMachine() {
        String name = "FIXED_STAR";
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_POWER_STOP.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        Assertions.assertTrue(vSphereHttpClient.updateVirtualMachinePowerState(name,
                VirtualMachinePowerState.POWERED_OFF));
    }

    @Test
    void getVirtualMachinePower() {
        String goodVmPowerResponseString = "{\n"
                + "    \"value\": \n"
                + "        {\n"
                + "            \"state\": \"POWERED_ON\"\n"
                + "        }"
                + "}";

        String name = "Marionette";
        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_POWER.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(goodVmPowerResponseString).withStatus(200)));

        Assertions.assertEquals(VirtualMachinePowerState.POWERED_ON, vSphereHttpClient.getVirtualMachinePower(name));
    }

    @Test
    void getVirtualMachinePowerBadResponse() {
        String name = "More_One_Night";

        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_POWER.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(404)));

        Assertions.assertThrows(
                PersephoneException.class,
            () -> vSphereHttpClient.getVirtualMachinePower(name)
        );
    }

    @Test
    void ensureVirtualMachinePowerStart() {
        String name = "promenade";

        String goodVmPowerResponseString = "{\n"
                + "    \"value\": \n"
                + "        {\n"
                + "            \"state\": \"POWERED_ON\"\n"
                + "        }"
                + "}";

        // Turn on VM Power
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_POWER_START.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        // Get power status
        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_POWER.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(goodVmPowerResponseString).withStatus(200)));

        // Update VM memory
        server.stubFor(patch(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_MEMORY_UPDATE.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        // Update VM CPU
        server.stubFor(patch(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_CPU_UPDATE.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        // Create VM Disk
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_DISK_CREATE.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        Map<String, String> properties = new HashMap<String, String>();

        properties.put(DeploymentAttributes.VM_MEMORY.toString(), "16");
        properties.put(DeploymentAttributes.VM_CPU_COUNT.toString(), "32");
        properties.put(DeploymentAttributes.VM_CORES_PER_SOCKET.toString(), "64");
        properties.put(DeploymentAttributes.VM_STORAGE.toString(), "67");

        long retryInterval = 10;
        Assertions.assertTrue(vSphereHttpClient.ensureVirtualMachinePowerStart(name, retryInterval,
                properties));
    }

    @Test
    void ensureVirtualMachinePowerStop() {
        String name = "Love_Marginal";

        String goodVmPowerResponseString = "{\n"
                + "    \"value\": \n"
                + "        {\n"
                + "            \"state\": \"POWERED_OFF\"\n"
                + "        }"
                + "}";

        // Turn off VM Power
        server.stubFor(post(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_POWER_STOP.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withStatus(200)));

        // Get power status
        server.stubFor(get(urlPathEqualTo(VsphereEndpoints.VSPHERE_VM_POWER.getPath().replace("{vm}", name)))
                .willReturn(aResponse()
                        .withHeader("Content-Type", "application/json")
                        .withBody(goodVmPowerResponseString).withStatus(200)));

        Assertions.assertTrue(vSphereHttpClient.ensureVirtualMachinePowerStop(name));
    }
}