/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import static com.vmware.blockchain.services.blockchains.zones.Zone.Type.ON_PREM;

import java.util.UUID;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;

/**
 * Test utilities for Zone.
 */
public class ZoneTestUtils {
    private static final String tlsCertDataForTest = "-----BEGIN CERTIFICATE-----\n"
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

    /**
     * create an on Prem zone.
     */
    public static OnPremZone getOnpremZone(UUID id, UUID orgId) {
        // Now construct an onprem zone
        OnPremZone ozone = new OnPremZone();
        ozone.setId(id);
        ozone.setType(ON_PREM);
        ozone.setName("On Prem");
        ozone.setOrgId(orgId);
        ozone.setVCenter(new OnPremZone.EndPoint("http://vcenter", "admin", "password"));
        ozone.setResourcePool("pool");
        ozone.setStorage("datastore");
        ozone.setFolder("folder");
        ozone.setNetwork(Zone.Network.builder()
                                 .name("Network 1")
                                 .ipPool(ImmutableList.of("10.1.1.16-10.1.1.64", "10.1.1.100-10.1.1.200"))
                                 .subnet("24")
                                 .gateway("10.1.1.1")
                                 .nameServers(ImmutableList.of("10.1.1.3"))
                                 .build());
        ozone.setOutboundProxy(Zone.OutboundProxy.builder()
                                       .httpHost("localhost")
                                       .httpPort(8080)
                                       .httpsHost("localhosts")
                                       .httpsPort(443).build());
        ozone.setLogManagements(Lists.newArrayList(OnPremZone.LogManagementOnPrem.builder()
                                                    .destination(Zone.LogDestination.LOG_INSIGHT)
                                                    .address("10.78.0.1")
                                                    .port(9000)
                                                    .username("foo")
                                                    .password("bar")
                                                    .build()));
        ozone.setContainerRepo(new OnPremZone.ContainerRepo("docker-repo", "user", "docker",
                                                            ZoneTestUtils.tlsCertDataForTest));

        ozone.setNotaryServer(new OnPremZone.NotaryServer("https://notary.test.com",
                                                          ZoneTestUtils.tlsCertDataForTest));

        return ozone;
    }

    public static OnPremZone getOnpremZone(UUID orgId) {
        return getOnpremZone(null, orgId);
    }



}
