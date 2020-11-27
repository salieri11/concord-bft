/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import com.vmware.blockchain.castor.model.DeploymentDescriptorModel;
import com.vmware.blockchain.castor.model.InfrastructureDescriptorModel;
import com.vmware.blockchain.castor.model.ProvisionDescriptorDescriptorModel;

/**
 * Descriptor test utils.
 */
public class DescriptorTestUtills {

    private static final String CONSORTIUM_NAME = "consortium-1";
    private static final String ZONE_1_NAME = "test-zone-1 - A";

    private static final String VC_URL = "https://vcenter.sddc.vmware.com/";
    // Multiple certs in single field scenario
    private static final String VC_CERT_DATA = "-----BEGIN CERTIFICATE-----\n"
                                               + "MIIENTCCAx2gAwIBAgIJANM+eIBn98ffMA0GCSqGSIb3DQEBCwUAMIGlMQswCQYD\n"
                                               + "VQQDDAJDQTEXMBUGCgmSJomT8ixkARkWB3ZzcGhlcmUxFTATBgoJkiaJk/IsZAEZ\n"
                                               + "FgVsb2NhbDELMAkGA1UEBhMCVVMxEzARBgNVBAgMCkNhbGlmb3JuaWExJzAlBgNV\n"
                                               + "BAoMHnNjMi0xMC0xODYtNi00NC5lbmcudm13YXJlLmNvbTEbMBkGA1UECwwSVk13\n"
                                               + "YXJlIEVuZ2luZWVyaW5nMB4XDTIwMTExNjAzMDMwMloXDTMwMTExNDAzMDMwMlow\n"
                                               + "gaUxCzAJBgNVBAMMAkNBMRcwFQYKCZImiZPyLGQBGRYHdnNwaGVyZTEVMBMGCgmS\n"
                                               + "JomT8ixkARkWBWxvY2FsMQswCQYDVQQGEwJVUzETMBEGA1UECAwKQ2FsaWZvcm5p\n"
                                               + "YTEnMCUGA1UECgwec2MyLTEwLTE4Ni02LTQ0LmVuZy52bXdhcmUuY29tMRswGQYD\n"
                                               + "VQQLDBJWTXdhcmUgRW5naW5lZXJpbmcwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAw\n"
                                               + "ggEKAoIBAQDG7zaMPCe7UAX55d6BeWOqhzfUNMxWVfE0iQ7j+TPu//j++VIYtNOF\n"
                                               + "3mP9WwUV9cCmcAqa5H+eV576bGr+rGV9wKbosyfs6w7VlcMoxYmrWhHQRafRWSZM\n"
                                               + "lLnpjDD4hlQwSNy9FlgBH3dotA7ejaRudJ9olkmIaiUVHGc2RrcXBmrZ67URTWI7\n"
                                               + "rGgHdW3rNSAZYpWNkn+GhMWS3xgKO2ypNIkT1kj6W7TC2kebklxqf+gJkf69W/bJ\n"
                                               + "99gL11s8mpJq/p7yakKM08GcOPddDDFDkGDg3CX6OlsaH63evQSMe5gBXI2E+sGF\n"
                                               + "vYalVCV8uxqM5kejeJ6hUowivL9X8gZvAgMBAAGjZjBkMB0GA1UdDgQWBBRlPNp1\n"
                                               + "Ad1EyBneGMocU2d4dsGSSDAfBgNVHREEGDAWgQ5lbWFpbEBhY21lLmNvbYcEfwAA\n"
                                               + "ATAOBgNVHQ8BAf8EBAMCAQYwEgYDVR0TAQH/BAgwBgEB/wIBADANBgkqhkiG9w0B\n"
                                               + "AQsFAAOCAQEAAjlr76K+ps0czTPj2ke3PfCAbcuK94nTPUo0HzChnwCc23qrsw3l\n"
                                               + "2oFxPbI16T3fY/W57ee7iI1j1/2//7byB1eRUnAdY7nlCqfUn/LxAgolHs5VX2FG\n"
                                               + "Ca0QnGAQFjzY3sdg/ETh68tpwO4tMCC9lRFcYgwaC/th+VniTOStvtplEjeMq3e9\n"
                                               + "ARC+wgPLQUiSsS3xEr5MQlDkXU8QJn8vEzOQ4hMzUumvaGVNFiKFWydmfGqbvlmV\n"
                                               + "V0Y/BZcR8gCQWiwqfUyIDVYMTapnBIiyPDRuJ/ZPA6wtvHKDP4pJSN+l+aJg+k5q\n"
                                               + "srQuJ/kl/YKiFS7R5X4/mJ4/PFYWiK316Q==\n"
                                               + "-----END CERTIFICATE-----\n"
                                               + "-----BEGIN CERTIFICATE-----\n"
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
                                               + "-----END CERTIFICATE-----\n";
    private static final String VC_USER = "admin@vsphere.local";
    private static final String VC_PWD = "The rain in Spain falls mainly on the plain";
    private static final String VC_RESOURCE_POOL = "testresourcepool";
    private static final String VC_STORAGE = "teststorage";
    private static final String VC_FOLDER = "testfolder";
    private static final String VC_NETWORK_NAME = "testnetwork";
    private static final String GATEWAY = "10.20.30.1";
    private static final List<String> NAMESERVERS = List.of("1.1.1.1", "1.0.0.1");
    private static final String CONTAINER_REG_URL = "https://container.registry.com";
    private static final String CONTAINER_REG_USER = "registryUser";
    private static final String CONTAINER_REG_PASS = "registryPassword";
    private static final String CONTAINER_REG_CERT = "-----BEGIN CERTIFICATE-----\n"
                                                 + "MIIFmDCCA4CgAwIBAgIJAIsTLIadH5LEMA0GCSqGSIb3DQEBCwUAMHAxCzAJBgNV\n"
                                                 + "BAYTAlVTMQswCQYDVQQIDAJWQTERMA8GA1UEBwwIU29tZUNpdHkxEjAQBgNVBAoM\n"
                                                 + "CU15Q29tcGFueTETMBEGA1UECwwKTXlEaXZpc2lvbjEYMBYGA1UEAwwPd3d3LmNv\n"
                                                 + "bXBhbnkuY29tMB4XDTIwMTAyNzE5MTMwNFoXDTIxMTAyNzE5MTMwNFowcDELMAkG\n"
                                                 + "A1UEBhMCVVMxCzAJBgNVBAgMAlZBMREwDwYDVQQHDAhTb21lQ2l0eTESMBAGA1UE\n"
                                                 + "CgwJTXlDb21wYW55MRMwEQYDVQQLDApNeURpdmlzaW9uMRgwFgYDVQQDDA93d3cu\n"
                                                 + "Y29tcGFueS5jb20wggIiMA0GCSqGSIb3DQEBAQUAA4ICDwAwggIKAoICAQDcI5vx\n"
                                                 + "arLgItJ6riKTGiEdUFmeF2318ku/4hTSkiX3H4Nc1Xof46fsJFWUgoxlGeKYZ0pF\n"
                                                 + "f8fDVqYwWhE7uBHNSdK4H0hQE0kM1hxhIOk0+pgDp4bKEOqC6/rN6GPIVO9DIQ5H\n"
                                                 + "F15WqfKRg/yDPUmL3QyXyoX/zzX92MqHOfqmoluC/2fRzHP2Fuk99BIG3TWA3Q49\n"
                                                 + "IO2fLF0bD2yDG4LMh20jyT/oCm6pGWYcOhZE0r53T8Ny1zyV0DwgqtcLct0l3qzk\n"
                                                 + "xWjkalfDXCjn14ZgHj8hYe2QPrh6cntTXBkonteHEzEUZHfGdXiovABAx02M9Ea2\n"
                                                 + "qySLgHy2Xn6PAIiU+9xL9Yb6nMeX0qheXZP+wKGdXObZbFLESfD+6d0pJY+wZyQC\n"
                                                 + "VSgH2TpTxdwkbkZmEJ7IAsq+ZKCxiv/a4kglCBgtC6Mg5GYjoY/c7EaYjxE7q6n8\n"
                                                 + "km0zLvYsBeRwD4Cui+0o/eh4lSuibiwO4VGzEjjTX9+DpT+kEdhhrE3QlTZtNgnO\n"
                                                 + "kYcanOJf/d5Ype7gx1py/gHlhHzpr22tf5f1x0AvWEq2nNMHuCoE05xPnDyOykRG\n"
                                                 + "qrJM8twixMc98fYkevZ3EgWdsTVhzOLlAEsqwgNDdr9Khx7dZiEtE/qEYdUxiJ4i\n"
                                                 + "L6HwyMLW/7AX1PbY8tqVbWS4hcL2appyqbGGqQIDAQABozUwMzALBgNVHQ8EBAMC\n"
                                                 + "BDAwEwYDVR0lBAwwCgYIKwYBBQUHAwEwDwYDVR0RBAgwBocECkjaBzANBgkqhkiG\n"
                                                 + "9w0BAQsFAAOCAgEAuhiCngHGpwKOVRrhowzbkBAYP9t/Vz9cONGZd0bCPstHl83q\n"
                                                 + "Ksbi6ptUQFHgilNhCjVCVA3TdVGAo7OF7i+svjoFLu5iOpLnrLtMYJtnPwwDJBRK\n"
                                                 + "I55nUnXIdvKmbp614k/YDB5wL7oX5DRgg1R5jwVT5EhiJDAlz+Q7yUxiE/qBOS+j\n"
                                                 + "7aw2wpSMKtpZAXgHPOTkedP6byJvWjx7s+AHInt5RiVx575WMLxJlbz+QMmn4olM\n"
                                                 + "vtwGUkRsGsZSbZiY7zCMHw8ObdtzFImcG5ZHNQUG9MI3uvMsxmgBn9bHUdIJSV+8\n"
                                                 + "MDQIYeXzDhNzLLgAyLNdBuwW2BjdFSycK8gEPilB/VSyke2FgIs41yNMI3T7gBfw\n"
                                                 + "2beclpaX/rHNlZidHk8lQTRGCRw1ydwkQcvoTXOeMrh+bv0FVdPX1RNbrjdunn5P\n"
                                                 + "QOXkQKSLw52+i+Omm8fDfIWffmyCYSVc+Z16Txvbze287p5DvWluolxALrqopokZ\n"
                                                 + "+GU8dlQi2WnVcih6iXS1QKOi2OyA9Rsar/BwqewSGxaoYU+9reatHXSdIi9q74Ol\n"
                                                 + "e9Vo45Zrt1tpy4A5RJAN2k9Va3iLH2R+hgRvFYvZRTtyHgFvdneG5WGOQDAOuK9U\n"
                                                 + "RmInjvwIH/s6yinHZb0X28kBVvqbyODJ5tGBoXFsE94IwBl8w/bScx9NF/I=\n"
                                                 + "-----END CERTIFICATE-----";
    private static final String NOTARY_SERVER_URL = "https://notary.vdp.test.com";
    private static final String NOTARY_CERT_DATA = "-----BEGIN CERTIFICATE-----\n"
                                               + "MIIFhjCCA26gAwIBAgIJAMDPQyyFDvTLMA0GCSqGSIb3DQEBCwUAMF8xCzAJBgNV\n"
                                               + "BAYTAlVTMQswCQYDVQQIDAJDQTEWMBQGA1UEBwwNU2FuIEZyYW5jaXNjbzEPMA0G\n"
                                               + "A1UECgwGRG9ja2VyMRowGAYDVQQDDBFOb3RhcnkgVGVzdGluZyBDQTAeFw0xOTAz\n"
                                               + "MTMwMzM4MzBaFw0yOTAzMTAwMzM4MzBaMF8xCzAJBgNVBAYTAlVTMQswCQYDVQQI\n"
                                               + "DAJDQTEWMBQGA1UEBwwNU2FuIEZyYW5jaXNjbzEPMA0GA1UECgwGRG9ja2VyMRow\n"
                                               + "GAYDVQQDDBFOb3RhcnkgVGVzdGluZyBDQTCCAiIwDQYJKoZIhvcNAQEBBQADggIP\n"
                                               + "ADCCAgoCggIBALhYY5zNWlDlHIgNhQ2PCDUxZYe9IL8OuIVQMrfbihD5Y16wNBRs\n"
                                               + "S+LgADFoLuOqk2+46A84kFPfUdsAzj+RME2MvhscJ06TsmWRc86GG+YWTtBR87cA\n"
                                               + "A/HTSTrKgRmy4wOYn3sLhjhuFENPZLMnAcLb+SW1OXNyirLOmL4U3DUERpliYgjp\n"
                                               + "wpXlWiq2eS/txhzTDd3+Js6FwWq61PxFxf3A5snz4h9FlCP17tRfeBxIseCfDGRl\n"
                                               + "fSWiCnpl9rRWINtwkViyz6V2ik1VPZdatoWIiH1+PnFREwCxp42dZopH8hqr3Vlk\n"
                                               + "Grtro+cp5p3s/QCrYWx7hAieLqUX1MXpR69PoOqggmJADRPvTlUeSjesIMkHyzVd\n"
                                               + "wAlgQWUlBG5MLjmmj5Qu0oeYzPRojG0bvkp4eX0NCT2cjNi0tAnVoDaHKabaU1V+\n"
                                               + "Hau1X6/jv/G88R4lHujKOmVdbVFw+Wsh9JcRm7YBhL9v3XJD7gF2Yzl+3Dst9EZn\n"
                                               + "T1fEkf2cmatxKCzcHENqJ7q/nZbaThHSVZ6p9b13wkdzRVHd5ZIRXh8R/hAKtXPT\n"
                                               + "8PeVsIPWmMmtFQdwytOGB/K6Zt3azd73MezRIIQmVTKzAxXMAI/20eiiKVTSC+/4\n"
                                               + "Y/sb9jp/6QlKm7+XItXgH7Us3e1TrFU0hJ3pXskBuDdFTsM4BnXBSh8DAgMBAAGj\n"
                                               + "RTBDMBIGA1UdEwEB/wQIMAYBAf8CAQEwDgYDVR0PAQH/BAQDAgFGMB0GA1UdDgQW\n"
                                               + "BBRUPtrEw+QIsXMuw9jkngUmzBR3QjANBgkqhkiG9w0BAQsFAAOCAgEAE65LEkhz\n"
                                               + "acwPiKhnTAWXGNANTYN2vbo+RxolqEbIfFWp0mQNYPZG9KwpR7r5R7U9yOjgQgMd\n"
                                               + "9jC6rbJiFmu8IhLUvWuuhDAQqw+FaUFyvswmUVKXbsxt9Y1uzgBhAbyS5Cqxcmlv\n"
                                               + "0b/emiiUO/wBiar2SJzJ+YNAW54ncllYdEU6m/rxpTujW4SV9fIzPngHyaQza4Y7\n"
                                               + "hH6H8qF/FBT9ljcTdTcZFPpjJn6EFhdf8rCSDe5VQ6SpKUzR7R/cSJWKrfsp40aw\n"
                                               + "jRj2oVPVPs1mAHummr8Ti7m6ozkfsrO2p0cX8xImKvr7AGenRu4cMk1iSH3GHCDC\n"
                                               + "/x2Bmw0uIQqh8dFU22273LvWEfyAdbjsTvCjlG04aUHPyKHAluUo5FdJBTZ33uMp\n"
                                               + "R0C3cKK2is9tHc3d9kTtQpA3dhvgx6CR4ZHSY0++YRyx5RA/RyxWNx1xsj0G6tAr\n"
                                               + "iOJGyea1H1IP3GWnDDFMmlGl5WwabGO3PB5crvWEyd1fZz3PZHszuKerR4VgQT7z\n"
                                               + "tNifnqUcmvxrXBKZ6PEJX9YDNShnmmKpiN0laZzsegC/f5t+i6GGBSuxDgQqyWkp\n"
                                               + "jSP6sJG/ji3EHCaPJi4ATvYsM5/JXIlyDdp4DwFF0dhP/6GbJJR29Hf2zFXPuq3h\n"
                                               + "H3I4sgD+sG9mrIOo2mrK3aQOD2j7YVxcgB8=\n"
                                               + "-----END CERTIFICATE-----";

    public static final String ROREPLICA_ACCESS_KEY = "ACCESS_KEY_1";
    public static final String ROREPLICA_BUCKET = "BUCKET_1";
    public static final String ROREPLICA_PROTOCOL = "s3";
    public static final String ROREPLICA_SECRET_KEY = "SECRET_KEY_1";
    public static final String ROREPLICA_URL = "blockchain.s3.amazonaws.com";

    private static Map<String, UUID> consortiumNameToUUIDMap = new HashMap<>();

    private static List<String> zones = List.of(ZONE_1_NAME, ZONE_1_NAME, ZONE_1_NAME, ZONE_1_NAME);

    static {
        consortiumNameToUUIDMap.put(CONSORTIUM_NAME, UUID.randomUUID());
    }

    /**
     * Build the Organization model.
     * @return the model
     */
    public static InfrastructureDescriptorModel.Organization buildOrganization() {
        InfrastructureDescriptorModel.Organization organization = InfrastructureDescriptorModel.Organization.builder()
                // .dockerImage("dockerImage v1")
                .damlSdk("1.0.1")
                // .templateId(UUID.randomUUID())
                .generatePassword(true)
                .generateDamlDbPassword(true)
                .build();

        return organization;
    }

    private static InfrastructureDescriptorModel.VCenter buildVCenter() {
        URL vcUrl;
        try {
            vcUrl = new URL(VC_URL);
        } catch (MalformedURLException e) {
            throw new Error(e);
        }

        return InfrastructureDescriptorModel.VCenter.builder()
                .url(vcUrl)
                .tlsCertificateData(VC_CERT_DATA)
                .userName(VC_USER)
                .password(VC_PWD)
                .resourcePool(VC_RESOURCE_POOL)
                .storage(VC_STORAGE)
                .folder(VC_FOLDER)
                .build();
    }

    private static InfrastructureDescriptorModel.Network buildNetwork() {
        return InfrastructureDescriptorModel.Network.builder()
                .name(VC_NETWORK_NAME)
                .gateway(GATEWAY)
                .subnet(24)
                .nameServers(NAMESERVERS)
                .build();
    }

    private static InfrastructureDescriptorModel.ContainerRegistry buildContainerRegistry() {
        URL containerRegistryUrl;
        try {
            containerRegistryUrl = new URL(CONTAINER_REG_URL);
        } catch (MalformedURLException e) {
            throw new Error(e);
        }

        return InfrastructureDescriptorModel.ContainerRegistry.builder()
                .url(containerRegistryUrl)
                .userName(CONTAINER_REG_USER)
                .password(CONTAINER_REG_PASS)
                .tlsCertificateData(CONTAINER_REG_CERT)
                .build();
    }

    private static InfrastructureDescriptorModel.NotaryServer buildNotaryServer() {
        URL notaryServerUrl;
        try {
            notaryServerUrl = new URL(NOTARY_SERVER_URL);
        } catch (MalformedURLException e) {
            throw new Error(e);
        }
        return InfrastructureDescriptorModel.NotaryServer.builder()
                .url(notaryServerUrl)
                .tlsCertificateData(NOTARY_CERT_DATA)
                .build();
    }

    private static InfrastructureDescriptorModel.Zone buildZone(String zoneName) {

        InfrastructureDescriptorModel.VCenter vCenter = buildVCenter();
        InfrastructureDescriptorModel.Network network = buildNetwork();
        InfrastructureDescriptorModel.NotaryServer notaryServer = buildNotaryServer();
        InfrastructureDescriptorModel.ContainerRegistry containerRegistry = buildContainerRegistry();

        InfrastructureDescriptorModel.Zone zone = InfrastructureDescriptorModel.Zone.builder()
                .name(zoneName)
                .vCenter(vCenter)
                .network(network)
                .containerRegistry(containerRegistry)
                .notaryServer(notaryServer)
                // skip outbound proxy
                // skip Wavefront
                // skip ElasticSearch
                // skip LogManagement
                .build();
        return zone;
    }

    /**
     * Build the infra descriptor model.
     * @return the model
     */
    public static InfrastructureDescriptorModel buildInfraDescriptorModel() {
        InfrastructureDescriptorModel.Zone zone1 = buildZone(ZONE_1_NAME);
        InfrastructureDescriptorModel.Organization organization = buildOrganization();
        return InfrastructureDescriptorModel.builder()
                .organization(organization)
                .zones(List.of(zone1))
                .build();
    }

    /**
     * Build the deployment descriptor model for read-only replica specs.
     */
    public static ProvisionDescriptorDescriptorModel buildReadOnlyReplicaDeploymentDescriptorModel() {
        // Build the base model
        ProvisionDescriptorDescriptorModel baseModel = buildDeploymentDescriptorModel();
        // Add read-only replica atuff
        List<DeploymentDescriptorModel.ReadonlyReplica> readonlyReplicas =
                zones.stream()
                        .map(n -> DeploymentDescriptorModel.ReadonlyReplica.builder()
                                .zoneName(n)
                                .bucketName(ROREPLICA_BUCKET)
                                .accessKey(ROREPLICA_ACCESS_KEY)
                                .secretKey(ROREPLICA_SECRET_KEY)
                                .protocol(ROREPLICA_PROTOCOL)
                                .url(ROREPLICA_URL)
                                .build()
                        )
                        .collect(Collectors.toList());

        // Update the base model
        return ProvisionDescriptorDescriptorModel.builder()
                .blockchain(baseModel.getBlockchain())
                .clients(baseModel.getClients())
                .replicas(baseModel.getReplicas())
                .replicaNodeSpec(baseModel.getReplicaNodeSpec())
                .readonlyReplicas(readonlyReplicas)
                .readonlyReplicaNodeSpec(baseModel.getReplicaNodeSpec())
                .clientNodeSpec(baseModel.getClientNodeSpec())
                .build();
    }

    /**
     * Build the deployment descriptor model.
     * @return the model
     */
    public static ProvisionDescriptorDescriptorModel buildDeploymentDescriptorModel() {
        DeploymentDescriptorModel.NodeSpecification clientNodeSpec =
                DeploymentDescriptorModel.NodeSpecification.builder().cpuCount(4).memoryGb(32).diskSizeGb(100).build();

        DeploymentDescriptorModel.NodeSpecification replicaNodeSpec =
                DeploymentDescriptorModel.NodeSpecification.builder().cpuCount(2).memoryGb(16).diskSizeGb(64).build();

        DeploymentDescriptorModel.Blockchain blockchain = DeploymentDescriptorModel.Blockchain.builder()
                .blockchainType(DeploymentDescriptorModel.BlockchainType.DAML)
                .consortiumName(CONSORTIUM_NAME)
                .build();

        DeploymentDescriptorModel.Client client1 = DeploymentDescriptorModel.Client.builder()
                .zoneName(ZONE_1_NAME)
                .groupName("Group1")
                .build();

        DeploymentDescriptorModel.Client client2 = DeploymentDescriptorModel.Client.builder()
                .zoneName(ZONE_1_NAME)
                .groupName("Group2")
                .build();

        DeploymentDescriptorModel.Client client3 = DeploymentDescriptorModel.Client.builder()
                .zoneName(ZONE_1_NAME)
                .groupName("Group1")
                .build();

        List<DeploymentDescriptorModel.Replica> replicas =
                zones.stream()
                        .map(n -> DeploymentDescriptorModel.Replica.builder().zoneName(n).build())
                        .collect(Collectors.toList());


        // Add Client and Replicas nodeSpec to deployment model.
        return ProvisionDescriptorDescriptorModel.builder()
                .blockchain(blockchain)
                .clients(List.of(client1, client2, client3))
                .replicas(replicas)
                .replicaNodeSpec(replicaNodeSpec)
                .clientNodeSpec(clientNodeSpec)
                .build();
    }

    /**
     * Build the deployment descriptor model.
     * @return the model
     */
    public static ProvisionDescriptorDescriptorModel buildDeploymentDescriptorModelWithClientTls() {
        DeploymentDescriptorModel.NodeSpecification clientNodeSpec =
                DeploymentDescriptorModel.NodeSpecification.builder().cpuCount(4).memoryGb(32).diskSizeGb(100).build();

        DeploymentDescriptorModel.NodeSpecification replicaNodeSpec =
                DeploymentDescriptorModel.NodeSpecification.builder().cpuCount(2).memoryGb(16).diskSizeGb(64).build();

        DeploymentDescriptorModel.Blockchain blockchain = DeploymentDescriptorModel.Blockchain.builder()
                .blockchainType(DeploymentDescriptorModel.BlockchainType.DAML)
                .consortiumName(CONSORTIUM_NAME)
                .build();

        DeploymentDescriptorModel.Client client1 = DeploymentDescriptorModel.Client.builder()
                .zoneName(ZONE_1_NAME)
                .groupName("Group1")
                .tlsLedgerData(DeploymentDescriptorModel.TlsLedgerData.builder()
                                   .cacrt("cacrt1")
                                   .pem("pem1")
                                   .crt("crt1")
                                   .clientAuth(DeploymentDescriptorModel.TlsLedgerData.ClientAuth.NONE).build())
                .build();

        DeploymentDescriptorModel.Client client2 = DeploymentDescriptorModel.Client.builder()
                .zoneName(ZONE_1_NAME)
                .groupName("Group2")
                .tlsLedgerData(DeploymentDescriptorModel.TlsLedgerData.builder()
                                   .cacrt("cacrt1")
                                   .pem("pem1")
                                   .crt("crt1")
                                   .clientAuth(DeploymentDescriptorModel.TlsLedgerData.ClientAuth.OPTIONAL).build())
                .build();

        DeploymentDescriptorModel.Client client3 = DeploymentDescriptorModel.Client.builder()
                .zoneName(ZONE_1_NAME)
                .groupName("Group1")
                .tlsLedgerData(DeploymentDescriptorModel.TlsLedgerData.builder()
                                   .cacrt("cacrt1")
                                   .pem("pem1")
                                   .crt("crt1")
                                   .clientAuth(DeploymentDescriptorModel.TlsLedgerData.ClientAuth.REQUIRE).build())
                .build();

        List<String> zones = List.of(ZONE_1_NAME, ZONE_1_NAME, ZONE_1_NAME, ZONE_1_NAME);

        List<DeploymentDescriptorModel.Replica> replicas =
                zones.stream()
                        .map(n -> DeploymentDescriptorModel.Replica.builder().zoneName(n).build())
                        .collect(Collectors.toList());

        // Add Client and Replicas nodeSpec to deployment model.
        return ProvisionDescriptorDescriptorModel.builder()
                .blockchain(blockchain)
                .clients(List.of(client1, client2, client3))
                .replicas(replicas)
                .clientNodeSpec(clientNodeSpec)
                .replicaNodeSpec(replicaNodeSpec)
                .build();
    }
}
