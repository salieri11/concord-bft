/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.contracts;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.post;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.util.UriComponentsBuilder;

import com.github.tomakehurst.wiremock.WireMockServer;

@ExtendWith(SpringExtension.class)
class CompilerTest {

    private static final String COMPILE_API = "/api/v1/contracts";

    private static WireMockServer server;

    private static String compileUrl;

    String goodCompileReturn =
            "{\"data\": "
            + "  ["
            + "      {"
            + "         \"contract_name\": \"contract\","
            + "         \"bytecode\": \"code\","
            + "         \"metadata\": \"{\\\"key\\\": \\\"value\\\"}\""
            + "      }"
            + "   ]"
            + "}";

    String goodVerifyReturn =
            "{\"data\": "
            + "  {\"verified\": true}"
            + "}";

    @BeforeEach
    void setUp() {
        server = new WireMockServer(options().dynamicPort());
        server.start();
        compileUrl =
                UriComponentsBuilder.newInstance()
                        .host("localhost")
                        .port(server.port())
                        .scheme("http")
                        .path(COMPILE_API).toUriString();
    }

    @Test
    void compile() {
        server.resetAll();
        server.stubFor(post(urlEqualTo(COMPILE_API + "/compile"))
                                    .willReturn(
                                       aResponse().withHeader("Content-Type", "application/json")
                                               .withBody(goodCompileReturn).withStatus(200)));

        Compiler.Result r = Compiler.compile("Code", "1.1", compileUrl, false, 200);
        Assertions.assertTrue(r.isSuccess());
    }

    @Test
    void compileError() {
        server.resetAll();
        server.stubFor(post(urlEqualTo(COMPILE_API + "/compile"))
                               .willReturn(
                                       aResponse().withStatus(400)));

        Compiler.Result r = Compiler.compile("Code", "1.1", compileUrl, false, 200);
        Assertions.assertFalse(r.isSuccess());
        Assertions.assertNotNull(r.getStderr());
    }

    @Test
    void verify() {
        server.resetAll();
        server.stubFor(post(urlEqualTo(COMPILE_API + "/verify"))
                               .willReturn(
                                       aResponse().withHeader("Content-Type", "application/json")
                                               .withBody(goodVerifyReturn).withStatus(200)));
        Assertions.assertTrue(Compiler.verify("Code", "1.1", compileUrl, "code", "contract", false, 200));
    }

}