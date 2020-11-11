/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import static com.vmware.blockchain.security.MvcTestSecurityConfig.createContext;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Type.ON_PREM;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Type.VMC_AWS;
import static com.vmware.blockchain.services.blockchains.zones.ZoneTestUtils.getOnpremZone;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.authentication;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.text.MessageFormat;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteServiceGrpc.OrchestrationSiteServiceStub;
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteRequest;
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteResponse;
import com.vmware.blockchain.security.MvcTestSecurityConfig;
import com.vmware.blockchain.services.blockchains.BlockchainUtils;
import com.vmware.blockchain.services.blockchains.clients.Client;
import com.vmware.blockchain.services.blockchains.clients.ClientService;
import com.vmware.blockchain.services.blockchains.replicas.Replica;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.blockchains.zones.Zone.Wavefront;
import com.vmware.blockchain.services.blockchains.zones.ZoneController.DependentNodesGetResponse;
import com.vmware.blockchain.services.blockchains.zones.ZoneController.OnPremGetResponse;
import com.vmware.blockchain.services.blockchains.zones.ZoneController.ZoneListResponse;
import com.vmware.blockchain.services.blockchains.zones.ZoneController.ZoneResponse;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.VmbcRoles;

import io.grpc.stub.StreamObserver;

/**
 * Tests for the ZoneController.
 */
@ExtendWith({SpringExtension.class})
@WebMvcTest(controllers = { ZoneController.class })
@ContextConfiguration(classes = {MvcTestSecurityConfig.class, MvcConfig.class, ZoneConfig.class})
@ComponentScan(basePackageClasses =
        {ZoneController.class, HelenExceptionHandler.class})


class ZoneControllerTest {

    private static final UUID SITE_1 = UUID.fromString("84b9a0ed-c162-446a-b8c0-2e45755f3844");
    private static final UUID SITE_2 = UUID.fromString("275638a3-8860-4925-85de-c73d45cb7232");
    private static final UUID OP_SITE = UUID.fromString("2aa01247-dd51-4460-bba0-0a0934f0419e");
    private static final UUID OP_SITE2 = UUID.fromString("2aa01247-dd51-4460-bba0-0a0934f0419f");
    private static final UUID ORG_ID = UUID.fromString("9ecb07bc-482c-48f3-80d0-23c4f9514902");
    private static final UUID OPER_ORG = UUID.fromString("0b93680b-e99d-4f83-bcfd-e2d52c6ea697");

    private static final UUID MISSING_ZONE = UUID.fromString("fc0d250d-3d0c-42a3-b8a5-92ce07e058aa");
    private static final UUID NOT_FOUND_ZONE = UUID.fromString("6c03886b-fa5f-4b66-812b-ede8fb7a4e1c");
    private static final UUID ZONE_WITH_DEPENDENCIES = UUID.fromString("b402968d-62aa-483c-9621-5eec89a01cd9");

    private static final UUID DELETE_EMPTY_ZONE = UUID.fromString("184b9ead-4880-405b-be71-c801ff2e5a4f");
    private static final UUID DELETE_ZONE_WITH_REPLICA = UUID.fromString("00d3a7bc-47ea-4b78-aa57-9d6951265e9e");
    private static final UUID DELETE_ZONE_WITH_CLIENT = UUID.fromString("5cb50a70-9702-4cfc-ad99-819d1c859283");
    private static final UUID DELETE_ZONE_WITH_REPLICA_AND_CLIENT =
            UUID.fromString("070a2410-71ea-409b-a663-159f81514e5c");
    private static final UUID BLOCKCHAIN_ID = UUID.fromString("021a1640-06c3-4fb0-9b85-1b2ee450301d");
    private static final UUID CLIENT_ID_1 = UUID.fromString("0270a409-e73c-49ff-899e-d1df50b4f280");
    private static final UUID CLIENT_ID_2 = UUID.fromString("57331489-0161-423d-b32f-0c6fa1104ba2");
    private static final UUID REPLICA_ID_1 = UUID.fromString("fb2fa9cc-517f-4738-a760-9227d77f0a48");
    private static final UUID REPLICA_ID_2 = UUID.fromString("02818461-ce12-4b3b-9af8-b24da240f7df");

    private static final UUID CLIENT_GROUP_ID = UUID.fromString("050d3785-e2fc-4b59-9042-191da02a81a9");
    private static final String CLIENT_GROUP_NAME = "Test Group";


    private static final String POST_ONPREM_BODY = "{\n"
                                                   + "  \"name\": \"OnPrem\",\n"
                                                   + "  \"type\": \"ON_PREM\",\n"
                                                   + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                                   + "  \"vcenter\": {\n"
                                                   + "    \"url\": \"www.vcenter.com\",\n"
                                                   + "    \"username\": \"admin\",\n"
                                                   + "    \"password\": \"password\"\n"
                                                   + "  },\n"
                                                   + "  \"resource_pool\": \"pool\",\n"
                                                   + "  \"storage\": \"datastore\",\n"
                                                   + "  \"folder\": \"folder\",\n"
                                                   + "  \"network\": {\n"
                                                   + "    \"name\": \"Network 1\",\n"
                                                   + "    \"ip_pool\": [\n"
                                                   + "      \"10.1.1.16-10.1.1.64\",\n"
                                                   + "      \"10.1.1.100-10.1.1.200\"\n"
                                                   + "    ],\n"
                                                   + "    \"gateway\": \"10.1.1.1\",\n"
                                                   + "    \"subnet\": \"24\",\n"
                                                   + "    \"name_servers\": [\n"
                                                   + "      \"10.1.1.3\"\n"
                                                   + "    ]\n"
                                                   + "  },\n"
                                                   + "  \"outbound_proxy\": {\n"
                                                   + "    \"http_host\": \"localhost\",\n"
                                                   + "    \"http_port\": 8080\n"
                                                   + "  },\n"
                                                   + "  \"container_repo\": {\n"
                                                   + "    \"url\": \"https://docker-repo.com\",\n"
                                                   + "    \"username\": \"user\",\n"
                                                   + "    \"password\": \"docker\"\n"
                                                   + "  },\n"
                                                   + "  \"wavefront\": {\n"
                                                   + "    \"url\": \"https://wavefront.com\",\n"
                                                   + "    \"token\": \"token\"\n"
                                                   + "  },\n"
                                                   + "  \"notary_server\": {\n"
                                                   + "    \"url\": \"https://notary.test.com\"\n"
                                                   + "  },\n"
                                                   + "  \"log_managements\": [{\n"
                                                   + "    \"destination\": \"LOG_INSIGHT\",\n"
                                                   + "    \"address\": \"10.78.20.10\",\n"
                                                   + "    \"port\": 9000,\n"
                                                   + "    \"username\": \"foo\",\n"
                                                   + "    \"password\": \"bar\"\n"
                                                   + "  }]\n"
                                                   + "}";

    private static final String POST_ONPREM_BODY_BAD_REQUEST = "{\n"
                                                    + "  \"name\": \"OnPrem\",\n"
                                                    + "  \"type\": \"ON_PREM\",\n"
                                                    + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                                    + "  \"vcenter\": {\n"
                                                    + "    \"url\": \"www.vcenter.com\",\n"
                                                    + "    \"username\": \"admin\",\n"
                                                    + "    \"password\": \"password\"\n"
                                                    + "  },\n"
                                                    + "  \"resource_pool\": \"pool\",\n"
                                                    + "  \"storage\": \"datastore\",\n"
                                                    + "  \"folder\": \"folder\",\n"
                                                    + "  \"network\": {\n"
                                                    + "    \"name\": \"Network 1\",\n"
                                                    + "    \"ip_pool\": [\n"
                                                    + "      \"10.1.1.16-10.1.1.64\",\n"
                                                    + "      \"10.1.1.100-10.1.1.200\"\n"
                                                    + "    ],\n"
                                                    + "    \"gateway\": \"10.1.1.1\",\n"
                                                    + "    \"subnet\": \"24\",\n"
                                                    + "    \"name_servers\": [\n"
                                                    + "      \"10.1.1.3\"\n"
                                                    + "    ]\n"
                                                    + "  },\n"
                                                    + "  \"outbound_proxy\": {\n"
                                                    + "    \"http_host\": \"localhost\",\n"
                                                    + "    \"http_port\": \"8080a\"\n"
                                                    + "  },\n"
                                                    + "  \"container_repo\": {\n"
                                                    + "    \"url\": \"https://docker-repo.com\",\n"
                                                    + "    \"username\": \"user\",\n"
                                                    + "    \"password\": \"docker\"\n"
                                                    + "  },\n"
                                                    + "  \"wavefront\": {\n"
                                                    + "    \"url\": \"https://wavefront.com\",\n"
                                                    + "    \"token\": \"token\"\n"
                                                    + "  },\n"
                                                    + "  \"log_managements\": [{\n"
                                                    + "    \"destination\": \"LOG_INSIGHT\",\n"
                                                    + "    \"address\": \"10.78.20.10\",\n"
                                                    + "    \"port\": 9000,\n"
                                                    + "    \"username\": \"foo\",\n"
                                                    + "    \"password\": \"bar\"\n"
                                                    + "  }]\n"
                                                    + "}";

    private static final String POST_ONPREM_BODY_MISSING_NAME = "{\n"
                                                    + "  \"type\": \"ON_PREM\",\n"
                                                    + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                                    + "  \"vcenter\": {\n"
                                                    + "    \"url\": \"www.vcenter.com\",\n"
                                                    + "    \"username\": \"admin\",\n"
                                                    + "    \"password\": \"password\"\n"
                                                    + "  },\n"
                                                    + "  \"resource_pool\": \"pool\",\n"
                                                    + "  \"storage\": \"datastore\",\n"
                                                    + "  \"folder\": \"folder\",\n"
                                                    + "  \"network\": {\n"
                                                    + "    \"name\": \"Network 1\",\n"
                                                    + "    \"ip_pool\": [\n"
                                                    + "      \"10.1.1.16-10.1.1.64\",\n"
                                                    + "      \"10.1.1.100-10.1.1.200\"\n"
                                                    + "    ],\n"
                                                    + "    \"gateway\": \"10.1.1.1\",\n"
                                                    + "    \"subnet\": \"24\",\n"
                                                    + "    \"name_servers\": [\n"
                                                    + "      \"10.1.1.3\"\n"
                                                    + "    ]\n"
                                                    + "  },\n"
                                                    + "  \"outbound_proxy\": {\n"
                                                    + "    \"http_host\": \"localhost\",\n"
                                                    + "    \"http_port\": 8080\n"
                                                    + "  },\n"
                                                    + "  \"container_repo\": {\n"
                                                    + "    \"url\": \"https://docker-repo.com\",\n"
                                                    + "    \"username\": \"user\",\n"
                                                    + "    \"password\": \"docker\"\n"
                                                    + "  },\n"
                                                    + "  \"wavefront\": {\n"
                                                    + "    \"url\": \"https://wavefront.com\",\n"
                                                    + "    \"token\": \"token\"\n"
                                                    + "  },\n"
                                                    + "  \"log_managements\": [{\n"
                                                    + "    \"destination\": \"LOG_INSIGHT\",\n"
                                                    + "    \"address\": \"10.78.20.10\",\n"
                                                    + "    \"port\": 9000,\n"
                                                    + "    \"username\": \"foo\",\n"
                                                    + "    \"password\": \"bar\"\n"
                                                    + "  }]\n"
                                                    + "}";

    private static final String POST_ONPREM_BODY_MISSING_RESOURCE_POOL = "{\n"
                                                   + "  \"name\": \"OnPrem\",\n"
                                                   + "  \"type\": \"ON_PREM\",\n"
                                                   + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                                   + "  \"vcenter\": {\n"
                                                   + "    \"url\": \"www.vcenter.com\",\n"
                                                   + "    \"username\": \"admin\",\n"
                                                   + "    \"password\": \"password\"\n"
                                                   + "  },\n"
                                                   + "  \"resource_pool\": \"\",\n"
                                                   + "  \"storage\": \"datastore\",\n"
                                                   + "  \"folder\": \"folder\",\n"
                                                   + "  \"network\": {\n"
                                                   + "    \"name\": \"Network 1\",\n"
                                                   + "    \"ip_pool\": [\n"
                                                   + "      \"10.1.1.16-10.1.1.64\",\n"
                                                   + "      \"10.1.1.100-10.1.1.200\"\n"
                                                   + "    ],\n"
                                                   + "    \"gateway\": \"10.1.1.1\",\n"
                                                   + "    \"subnet\": \"24\",\n"
                                                   + "    \"name_servers\": [\n"
                                                   + "      \"10.1.1.3\"\n"
                                                   + "    ]\n"
                                                   + "  },\n"
                                                   + "  \"outbound_proxy\": {\n"
                                                   + "    \"http_host\": \"localhost\",\n"
                                                   + "    \"http_port\": 8080\n"
                                                   + "  },\n"
                                                   + "  \"container_repo\": {\n"
                                                   + "    \"url\": \"https://docker-repo.com\",\n"
                                                   + "    \"username\": \"user\",\n"
                                                   + "    \"password\": \"docker\"\n"
                                                   + "  },\n"
                                                   + "  \"wavefront\": {\n"
                                                   + "    \"url\": \"https://wavefront.com\",\n"
                                                   + "    \"token\": \"token\"\n"
                                                   + "  },\n"
                                                   + "  \"log_managements\": [{\n"
                                                   + "    \"destination\": \"LOG_INSIGHT\",\n"
                                                   + "    \"address\": \"10.78.20.10\",\n"
                                                   + "    \"port\": 9000,\n"
                                                   + "    \"username\": \"foo\",\n"
                                                   + "    \"password\": \"bar\"\n"
                                                   + "  }]\n"
                                                   + "}";

    private static final String POST_ONPREM_BODY_BAD_NETWORK = "{\n"
                                                    + "  \"name\": \"OnPrem\",\n"
                                                    + "  \"type\": \"ON_PREM\",\n"
                                                    + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                                    + "  \"vcenter\": {\n"
                                                    + "    \"url\": \"www.vcenter.com\",\n"
                                                    + "    \"username\": \"admin\",\n"
                                                    + "    \"password\": \"password\"\n"
                                                    + "  },\n"
                                                    + "  \"resource_pool\": \"pool\",\n"
                                                    + "  \"storage\": \"datastore\",\n"
                                                    + "  \"folder\": \"folder\",\n"
                                                    + "  \"network\": {\n"
                                                    + "    \"name\": \"Network 1\",\n"
                                                    + "    \"ip_pool\": [\n"
                                                    + "      \"\",\n"
                                                    + "      \"\"\n"
                                                    + "    ],\n"
                                                    + "    \"gateway\": \"10.1.1.1\",\n"
                                                    + "    \"subnet\": \"24\",\n"
                                                    + "    \"name_servers\": [\n"
                                                    + "      \"10.1.1.3\"\n"
                                                    + "    ]\n"
                                                    + "  },\n"
                                                    + "  \"outbound_proxy\": {\n"
                                                    + "    \"http_host\": \"localhost\",\n"
                                                    + "    \"http_port\": 8080\n"
                                                    + "  },\n"
                                                    + "  \"container_repo\": {\n"
                                                    + "    \"url\": \"https://docker-repo.com\",\n"
                                                    + "    \"username\": \"user\",\n"
                                                    + "    \"password\": \"docker\"\n"
                                                    + "  },\n"
                                                    + "  \"wavefront\": {\n"
                                                    + "    \"url\": \"https://wavefront.com\",\n"
                                                    + "    \"token\": \"token\"\n"
                                                    + "  },\n"
                                                    + "  \"log_managements\": [{\n"
                                                    + "    \"destination\": \"LOG_INSIGHT\",\n"
                                                    + "    \"address\": \"10.78.20.10\",\n"
                                                    + "    \"port\": 9000,\n"
                                                    + "    \"username\": \"foo\",\n"
                                                    + "    \"password\": \"bar\"\n"
                                                    + "  }]\n"
                                                    + "}";

    private static final String POST_ONPREM_BODY_BAD_VCENTER = "{\n"
                                                    + "  \"name\": \"OnPrem\",\n"
                                                    + "  \"type\": \"ON_PREM\",\n"
                                                    + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                                    + "  \"vcenter\": {\n"
                                                    + "    \"url\": \"\",\n"
                                                    + "    \"username\": \"admin\",\n"
                                                    + "    \"password\": \"password\"\n"
                                                    + "  },\n"
                                                    + "  \"resource_pool\": \"pool\",\n"
                                                    + "  \"storage\": \"datastore\",\n"
                                                    + "  \"folder\": \"folder\",\n"
                                                    + "  \"network\": {\n"
                                                    + "    \"name\": \"Network 1\",\n"
                                                    + "    \"ip_pool\": [\n"
                                                    + "      \"10.1.1.16-10.1.1.64\",\n"
                                                    + "      \"10.1.1.100-10.1.1.200\"\n"
                                                    + "    ],\n"
                                                    + "    \"gateway\": \"10.1.1.1\",\n"
                                                    + "    \"subnet\": \"24\",\n"
                                                    + "    \"name_servers\": [\n"
                                                    + "      \"10.1.1.3\"\n"
                                                    + "    ]\n"
                                                    + "  },\n"
                                                    + "  \"outbound_proxy\": {\n"
                                                    + "    \"http_host\": \"localhost\",\n"
                                                    + "    \"http_port\": 8080\n"
                                                    + "  },\n"
                                                    + "  \"container_repo\": {\n"
                                                    + "    \"url\": \"https://docker-repo.com\",\n"
                                                    + "    \"username\": \"user\",\n"
                                                    + "    \"password\": \"docker\"\n"
                                                    + "  },\n"
                                                    + "  \"wavefront\": {\n"
                                                    + "    \"url\": \"https://wavefront.com\",\n"
                                                    + "    \"token\": \"token\"\n"
                                                    + "  },\n"
                                                    + "  \"log_managements\": [{\n"
                                                    + "    \"destination\": \"LOG_INSIGHT\",\n"
                                                    + "    \"address\": \"10.78.20.10\",\n"
                                                    + "    \"port\": 9000,\n"
                                                    + "    \"username\": \"foo\",\n"
                                                    + "    \"password\": \"bar\"\n"
                                                    + "  }]\n"
                                                    + "}";

    public static final String VALID_TLS_CERT_DATA = "-----BEGIN CERTIFICATE-----\\n"
                                              + "MIIFhjCCA26gAwIBAgIJAMDPQyyFDvTLMA0GCSqGSIb3DQEBCwUAMF8xCzAJBgNV\\n"
                                              + "BAYTAlVTMQswCQYDVQQIDAJDQTEWMBQGA1UEBwwNU2FuIEZyYW5jaXNjbzEPMA0G\\n"
                                              + "A1UECgwGRG9ja2VyMRowGAYDVQQDDBFOb3RhcnkgVGVzdGluZyBDQTAeFw0xOTAz\\n"
                                              + "MTMwMzM4MzBaFw0yOTAzMTAwMzM4MzBaMF8xCzAJBgNVBAYTAlVTMQswCQYDVQQI\\n"
                                              + "DAJDQTEWMBQGA1UEBwwNU2FuIEZyYW5jaXNjbzEPMA0GA1UECgwGRG9ja2VyMRow\\n"
                                              + "GAYDVQQDDBFOb3RhcnkgVGVzdGluZyBDQTCCAiIwDQYJKoZIhvcNAQEBBQADggIP\\n"
                                              + "ADCCAgoCggIBALhYY5zNWlDlHIgNhQ2PCDUxZYe9IL8OuIVQMrfbihD5Y16wNBRs\\n"
                                              + "S+LgADFoLuOqk2+46A84kFPfUdsAzj+RME2MvhscJ06TsmWRc86GG+YWTtBR87cA\\n"
                                              + "A/HTSTrKgRmy4wOYn3sLhjhuFENPZLMnAcLb+SW1OXNyirLOmL4U3DUERpliYgjp\\n"
                                              + "wpXlWiq2eS/txhzTDd3+Js6FwWq61PxFxf3A5snz4h9FlCP17tRfeBxIseCfDGRl\\n"
                                              + "fSWiCnpl9rRWINtwkViyz6V2ik1VPZdatoWIiH1+PnFREwCxp42dZopH8hqr3Vlk\\n"
                                              + "Grtro+cp5p3s/QCrYWx7hAieLqUX1MXpR69PoOqggmJADRPvTlUeSjesIMkHyzVd\\n"
                                              + "wAlgQWUlBG5MLjmmj5Qu0oeYzPRojG0bvkp4eX0NCT2cjNi0tAnVoDaHKabaU1V+\\n"
                                              + "Hau1X6/jv/G88R4lHujKOmVdbVFw+Wsh9JcRm7YBhL9v3XJD7gF2Yzl+3Dst9EZn\\n"
                                              + "T1fEkf2cmatxKCzcHENqJ7q/nZbaThHSVZ6p9b13wkdzRVHd5ZIRXh8R/hAKtXPT\\n"
                                              + "8PeVsIPWmMmtFQdwytOGB/K6Zt3azd73MezRIIQmVTKzAxXMAI/20eiiKVTSC+/4\\n"
                                              + "Y/sb9jp/6QlKm7+XItXgH7Us3e1TrFU0hJ3pXskBuDdFTsM4BnXBSh8DAgMBAAGj\\n"
                                              + "RTBDMBIGA1UdEwEB/wQIMAYBAf8CAQEwDgYDVR0PAQH/BAQDAgFGMB0GA1UdDgQW\\n"
                                              + "BBRUPtrEw+QIsXMuw9jkngUmzBR3QjANBgkqhkiG9w0BAQsFAAOCAgEAE65LEkhz\\n"
                                              + "acwPiKhnTAWXGNANTYN2vbo+RxolqEbIfFWp0mQNYPZG9KwpR7r5R7U9yOjgQgMd\\n"
                                              + "9jC6rbJiFmu8IhLUvWuuhDAQqw+FaUFyvswmUVKXbsxt9Y1uzgBhAbyS5Cqxcmlv\\n"
                                              + "0b/emiiUO/wBiar2SJzJ+YNAW54ncllYdEU6m/rxpTujW4SV9fIzPngHyaQza4Y7\\n"
                                              + "hH6H8qF/FBT9ljcTdTcZFPpjJn6EFhdf8rCSDe5VQ6SpKUzR7R/cSJWKrfsp40aw\\n"
                                              + "jRj2oVPVPs1mAHummr8Ti7m6ozkfsrO2p0cX8xImKvr7AGenRu4cMk1iSH3GHCDC\\n"
                                              + "/x2Bmw0uIQqh8dFU22273LvWEfyAdbjsTvCjlG04aUHPyKHAluUo5FdJBTZ33uMp\\n"
                                              + "R0C3cKK2is9tHc3d9kTtQpA3dhvgx6CR4ZHSY0++YRyx5RA/RyxWNx1xsj0G6tAr\\n"
                                              + "iOJGyea1H1IP3GWnDDFMmlGl5WwabGO3PB5crvWEyd1fZz3PZHszuKerR4VgQT7z\\n"
                                              + "tNifnqUcmvxrXBKZ6PEJX9YDNShnmmKpiN0laZzsegC/f5t+i6GGBSuxDgQqyWkp\\n"
                                              + "jSP6sJG/ji3EHCaPJi4ATvYsM5/JXIlyDdp4DwFF0dhP/6GbJJR29Hf2zFXPuq3h\\n"
                                              + "H3I4sgD+sG9mrIOo2mrK3aQOD2j7YVxcgB8=\\n"
                                              + "-----END CERTIFICATE-----";

    public static final String INVALID_TLS_CERT_DATA = "-----BEGIN CERTIFICATE-----\\n"
                                            + "hjCCA26gAwIBAgIJAMDPQyyFDvTLMA0GCSqGSIb3DQEBCwUAMF8xCzAJBgNV\\n"
                                            + "BAYTAlVTMQswCQYDVQQIDAJDQTEWMBQGA1UEBwwNU2FuIEZyYW5jaXNjbzEPMA0G\\n"
                                            + "A1UECgwGRG9ja2VyMRowGAYDVQQDDBFOb3RhcnkgVGVzdGluZyBDQTAeFw0xOTAz\\n"
                                            + "MTMwMzM4MzBaFw0yOTAzMTAwMzM4MzBaMF8xCzAJBgNVBAYTAlVTMQswCQYDVQQI\\n"
                                            + "DAJDQTEWMBQGA1UEBwwNU2FuIEZyYW5jaXNjbzEPMA0GA1UECgwGRG9ja2VyMRow\\n"
                                            + "GAYDVQQDDBFOb3RhcnkgVGVzdGluZyBDQTCCAiIwDQYJKoZIhvcNAQEBBQADggIP\\n"
                                            + "ADCCAgoCggIBALhYY5zNWlDlHIgNhQ2PCDUxZYe9IL8OuIVQMrfbihD5Y16wNBRs\\n"
                                            + "S+LgADFoLuOqk2+46A84kFPfUdsAzj+RME2MvhscJ06TsmWRc86GG+YWTtBR87cA\\n"
                                            + "A/HTSTrKgRmy4wOYn3sLhjhuFENPZLMnAcLb+SW1OXNyirLOmL4U3DUERpliYgjp\\n"
                                            + "wpXlWiq2eS/txhzTDd3+Js6FwWq61PxFxf3A5snz4h9FlCP17tRfeBxIseCfDGRl\\n"
                                            + "fSWiCnpl9rRWINtwkViyz6V2ik1VPZdatoWIiH1+PnFREwCxp42dZopH8hqr3Vlk\\n"
                                            + "Grtro+cp5p3s/QCrYWx7hAieLqUX1MXpR69PoOqggmJADRPvTlUeSjesIMkHyzVd\\n"
                                            + "wAlgQWUlBG5MLjmmj5Qu0oeYzPRojG0bvkp4eX0NCT2cjNi0tAnVoDaHKabaU1V+\\n"
                                            + "Hau1X6/jv/G88R4lHujKOmVdbVFw+Wsh9JcRm7YBhL9v3XJD7gF2Yzl+3Dst9EZn\\n"
                                            + "T1fEkf2cmatxKCzcHENqJ7q/nZbaThHSVZ6p9b13wkdzRVHd5ZIRXh8R/hAKtXPT\\n"
                                            + "8PeVsIPWmMmtFQdwytOGB/K6Zt3azd73MezRIIQmVTKzAxXMAI/20eiiKVTSC+/4\\n"
                                            + "Y/sb9jp/6QlKm7+XItXgH7Us3e1TrFU0hJ3pXskBuDdFTsM4BnXBSh8DAgMBAAGj\\n"
                                            + "RTBDMBIGA1UdEwEB/wQIMAYBAf8CAQEwDgYDVR0PAQH/BAQDAgFGMB0GA1UdDgQW\\n"
                                            + "BBRUPtrEw+QIsXMuw9jkngUmzBR3QjANBgkqhkiG9w0BAQsFAAOCAgEAE65LEkhz\\n"
                                            + "acwPiKhnTAWXGNANTYN2vbo+RxolqEbIfFWp0mQNYPZG9KwpR7r5R7U9yOjgQgMd\\n"
                                            + "9jC6rbJiFmu8IhLUvWuuhDAQqw+FaUFyvswmUVKXbsxt9Y1uzgBhAbyS5Cqxcmlv\\n"
                                            + "0b/emiiUO/wBiar2SJzJ+YNAW54ncllYdEU6m/rxpTujW4SV9fIzPngHyaQza4Y7\\n"
                                            + "hH6H8qF/FBT9ljcTdTcZFPpjJn6EFhdf8rCSDe5VQ6SpKUzR7R/cSJWKrfsp40aw\\n"
                                            + "jRj2oVPVPs1mAHummr8Ti7m6ozkfsrO2p0cX8xImKvr7AGenRu4cMk1iSH3GHCDC\\n"
                                            + "/x2Bmw0uIQqh8dFU22273LvWEfyAdbjsTvCjlG04aUHPyKHAluUo5FdJBTZ33uMp\\n"
                                            + "R0C3cKK2is9tHc3d9kTtQpA3dhvgx6CR4ZHSY0++YRyx5RA/RyxWNx1xsj0G6tAr\\n"
                                            + "iOJGyea1H1IP3GWnDDFMmlGl5WwabGO3PB5crvWEyd1fZz3PZHszuKerR4VgQT7z\\n"
                                            + "tNifnqUcmvxrXBKZ6PEJX9YDNShnmmKpiN0laZzsegC/f5t+i6GGBSuxDgQqyWkp\\n"
                                            + "jSP6sJG/ji3EHCaPJi4ATvYsM5/JXIlyDdp4DwFF0dhP/6GbJJR29Hf2zFXPuq3h\\n"
                                            + "H3I4sgD+sG9mrIOo2mrK3aQOD2j7YVxcgB8=\\n"
                                            + "-----END CERTIFICATE-----";

    private static final String POST_ONPREM_BODY_CR_VALID_CERT = "{\n"
                                           + "  \"name\": \"OnPrem\",\n"
                                           + "  \"type\": \"ON_PREM\",\n"
                                           + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                           + "  \"vcenter\": {\n"
                                           + "    \"url\": \"www.vcenter.com\",\n"
                                           + "    \"username\": \"admin\",\n"
                                           + "    \"password\": \"password\"\n"
                                           + "  },\n"
                                           + "  \"resource_pool\": \"pool\",\n"
                                           + "  \"storage\": \"datastore\",\n"
                                           + "  \"folder\": \"folder\",\n"
                                           + "  \"network\": {\n"
                                           + "    \"name\": \"Network 1\",\n"
                                           + "    \"ip_pool\": [\n"
                                           + "      \"10.1.1.16-10.1.1.64\",\n"
                                           + "      \"10.1.1.100-10.1.1.200\"\n"
                                           + "    ],\n"
                                           + "    \"gateway\": \"10.1.1.1\",\n"
                                           + "    \"subnet\": \"24\",\n"
                                           + "    \"name_servers\": [\n"
                                           + "      \"10.1.1.3\"\n"
                                           + "    ]\n"
                                           + "  },\n"
                                           + "  \"outbound_proxy\": {\n"
                                           + "    \"http_host\": \"localhost\",\n"
                                           + "    \"http_port\": 8080\n"
                                           + "  },\n"
                                           + "  \"container_repo\": {\n"
                                           + "    \"url\": \"https://docker-repo.com\",\n"
                                           + "    \"username\": \"user\",\n"
                                           + "    \"password\": \"docker\",\n"
                                           + "    \"tls_certificate_data\": \"" + VALID_TLS_CERT_DATA + "\"\n"
                                           + "  },\n"
                                           + "  \"wavefront\": {\n"
                                           + "    \"url\": \"https://wavefront.com\",\n"
                                           + "    \"token\": \"token\"\n"
                                           + "  },\n"
                                           + "  \"notary_server\": {\n"
                                           + "    \"url\": \"https://notary.test.com\"\n"
                                           + " },\n"
                                           + "  \"log_managements\": [{\n"
                                           + "    \"destination\": \"LOG_INSIGHT\",\n"
                                           + "    \"address\": \"10.78.20.10\",\n"
                                           + "    \"port\": 9000,\n"
                                           + "    \"username\": \"foo\",\n"
                                           + "    \"password\": \"bar\"\n"
                                           + "  }]\n"
                                           + "}";

    private static final String POST_ONPREM_BODY_CR_INVALID_CERT = "{\n"
                                             + "  \"name\": \"OnPrem\",\n"
                                             + "  \"type\": \"ON_PREM\",\n"
                                             + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                             + "  \"vcenter\": {\n"
                                             + "    \"url\": \"www.vcenter.com\",\n"
                                             + "    \"username\": \"admin\",\n"
                                             + "    \"password\": \"password\"\n"
                                             + "  },\n"
                                             + "  \"resource_pool\": \"pool\",\n"
                                             + "  \"storage\": \"datastore\",\n"
                                             + "  \"folder\": \"folder\",\n"
                                             + "  \"network\": {\n"
                                             + "    \"name\": \"Network 1\",\n"
                                             + "    \"ip_pool\": [\n"
                                             + "      \"10.1.1.16-10.1.1.64\",\n"
                                             + "      \"10.1.1.100-10.1.1.200\"\n"
                                             + "    ],\n"
                                             + "    \"gateway\": \"10.1.1.1\",\n"
                                             + "    \"subnet\": \"24\",\n"
                                             + "    \"name_servers\": [\n"
                                             + "      \"10.1.1.3\"\n"
                                             + "    ]\n"
                                             + "  },\n"
                                             + "  \"outbound_proxy\": {\n"
                                             + "    \"http_host\": \"localhost\",\n"
                                             + "    \"http_port\": 8080\n"
                                             + "  },\n"
                                             + "  \"container_repo\": {\n"
                                             + "    \"url\": \"https://docker-repo.com\",\n"
                                             + "    \"username\": \"user\",\n"
                                             + "    \"password\": \"docker\",\n"
                                             + "    \"tls_certificate_data\": \"" + INVALID_TLS_CERT_DATA + "\"\n"
                                             + "  },\n"
                                             + "  \"wavefront\": {\n"
                                             + "    \"url\": \"https://wavefront.com\",\n"
                                             + "    \"token\": \"token\"\n"
                                             + "  },\n"
                                             + "  \"notary_server\": {\n"
                                             + "    \"url\": \"https://notary.test.com\"\n"
                                             + " },\n"
                                             + "  \"log_managements\": [{\n"
                                             + "    \"destination\": \"LOG_INSIGHT\",\n"
                                             + "    \"address\": \"10.78.20.10\",\n"
                                             + "    \"port\": 9000,\n"
                                             + "    \"username\": \"foo\",\n"
                                             + "    \"password\": \"bar\"\n"
                                             + "  }]\n"
                                             + "}";

    private static final String POST_ONPREM_BODY_NOTARY_VALID_CERT = "{\n"
                                               + "  \"name\": \"OnPrem\",\n"
                                               + "  \"type\": \"ON_PREM\",\n"
                                               + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                               + "  \"vcenter\": {\n"
                                               + "    \"url\": \"www.vcenter.com\",\n"
                                               + "    \"username\": \"admin\",\n"
                                               + "    \"password\": \"password\"\n"
                                               + "  },\n"
                                               + "  \"resource_pool\": \"pool\",\n"
                                               + "  \"storage\": \"datastore\",\n"
                                               + "  \"folder\": \"folder\",\n"
                                               + "  \"network\": {\n"
                                               + "    \"name\": \"Network 1\",\n"
                                               + "    \"ip_pool\": [\n"
                                               + "      \"10.1.1.16-10.1.1.64\",\n"
                                               + "      \"10.1.1.100-10.1.1.200\"\n"
                                               + "    ],\n"
                                               + "    \"gateway\": \"10.1.1.1\",\n"
                                               + "    \"subnet\": \"24\",\n"
                                               + "    \"name_servers\": [\n"
                                               + "      \"10.1.1.3\"\n"
                                               + "    ]\n"
                                               + "  },\n"
                                               + "  \"outbound_proxy\": {\n"
                                               + "    \"http_host\": \"localhost\",\n"
                                               + "    \"http_port\": 8080\n"
                                               + "  },\n"
                                               + "  \"container_repo\": {\n"
                                               + "    \"url\": \"https://docker-repo.com\",\n"
                                               + "    \"username\": \"user\",\n"
                                               + "    \"password\": \"docker\"\n"
                                               + "  },\n"
                                               + "  \"wavefront\": {\n"
                                               + "    \"url\": \"https://wavefront.com\",\n"
                                               + "    \"token\": \"token\"\n"
                                               + "  },\n"
                                               + "  \"notary_server\": {\n"
                                               + "    \"url\": \"https://notary.test.com\",\n"
                                               + "    \"tls_certificate_data\": \"" + VALID_TLS_CERT_DATA + "\"\n"
                                               + " },\n"
                                               + "  \"log_managements\": [{\n"
                                               + "    \"destination\": \"LOG_INSIGHT\",\n"
                                               + "    \"address\": \"10.78.20.10\",\n"
                                               + "    \"port\": 9000,\n"
                                               + "    \"username\": \"foo\",\n"
                                               + "    \"password\": \"bar\"\n"
                                               + "  }]\n"
                                               + "}";

    private static final String POST_ONPREM_BODY_NOTARY_INVALID_CERT = "{\n"
                                                + "  \"name\": \"OnPrem\",\n"
                                                + "  \"type\": \"ON_PREM\",\n"
                                                + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                                + "  \"vcenter\": {\n"
                                                + "    \"url\": \"www.vcenter.com\",\n"
                                                + "    \"username\": \"admin\",\n"
                                                + "    \"password\": \"password\"\n"
                                                + "  },\n"
                                                + "  \"resource_pool\": \"pool\",\n"
                                                + "  \"storage\": \"datastore\",\n"
                                                + "  \"folder\": \"folder\",\n"
                                                + "  \"network\": {\n"
                                                + "    \"name\": \"Network 1\",\n"
                                                + "    \"ip_pool\": [\n"
                                                + "      \"10.1.1.16-10.1.1.64\",\n"
                                                + "      \"10.1.1.100-10.1.1.200\"\n"
                                                + "    ],\n"
                                                + "    \"gateway\": \"10.1.1.1\",\n"
                                                + "    \"subnet\": \"24\",\n"
                                                + "    \"name_servers\": [\n"
                                                + "      \"10.1.1.3\"\n"
                                                + "    ]\n"
                                                + "  },\n"
                                                + "  \"outbound_proxy\": {\n"
                                                + "    \"http_host\": \"localhost\",\n"
                                                + "    \"http_port\": 8080\n"
                                                + "  },\n"
                                                + "  \"container_repo\": {\n"
                                                + "    \"url\": \"https://docker-repo.com\",\n"
                                                + "    \"username\": \"user\",\n"
                                                + "    \"password\": \"docker\"\n"
                                                + "  },\n"
                                                + "  \"wavefront\": {\n"
                                                + "    \"url\": \"https://wavefront.com\",\n"
                                                + "    \"token\": \"token\"\n"
                                                + "  },\n"
                                                + "  \"notary_server\": {\n"
                                                + "    \"url\": \"https://notary.test.com\",\n"
                                                + "    \"tls_certificate_data\": \"" + INVALID_TLS_CERT_DATA + "\"\n"
                                                + " },\n"
                                                + "  \"log_managements\": [{\n"
                                                + "    \"destination\": \"LOG_INSIGHT\",\n"
                                                + "    \"address\": \"10.78.20.10\",\n"
                                                + "    \"port\": 9000,\n"
                                                + "    \"username\": \"foo\",\n"
                                                + "    \"password\": \"bar\"\n"
                                                + "  }]\n"
                                                + "}";

    private static final String POST_ONPREM_BODY_NO_NOTARY_URL_BUT_CERT = "{\n"
                                                + "  \"name\": \"OnPrem\",\n"
                                                + "  \"type\": \"ON_PREM\",\n"
                                                + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                                + "  \"vcenter\": {\n"
                                                + "    \"url\": \"www.vcenter.com\",\n"
                                                + "    \"username\": \"admin\",\n"
                                                + "    \"password\": \"password\"\n"
                                                + "  },\n"
                                                + "  \"resource_pool\": \"pool\",\n"
                                                + "  \"storage\": \"datastore\",\n"
                                                + "  \"folder\": \"folder\",\n"
                                                + "  \"network\": {\n"
                                                + "    \"name\": \"Network 1\",\n"
                                                + "    \"ip_pool\": [\n"
                                                + "      \"10.1.1.16-10.1.1.64\",\n"
                                                + "      \"10.1.1.100-10.1.1.200\"\n"
                                                + "    ],\n"
                                                + "    \"gateway\": \"10.1.1.1\",\n"
                                                + "    \"subnet\": \"24\",\n"
                                                + "    \"name_servers\": [\n"
                                                + "      \"10.1.1.3\"\n"
                                                + "    ]\n"
                                                + "  },\n"
                                                + "  \"outbound_proxy\": {\n"
                                                + "    \"http_host\": \"localhost\",\n"
                                                + "    \"http_port\": 8080\n"
                                                + "  },\n"
                                                + "  \"container_repo\": {\n"
                                                + "    \"url\": \"https://docker-repo.com\",\n"
                                                + "    \"username\": \"user\",\n"
                                                + "    \"password\": \"docker\"\n"
                                                + "  },\n"
                                                + "  \"wavefront\": {\n"
                                                + "    \"url\": \"https://wavefront.com\",\n"
                                                + "    \"token\": \"token\"\n"
                                                + "  },\n"
                                                + "  \"notary_server\": {\n"
                                                + "    \"tls_certificate_data\": \"" + VALID_TLS_CERT_DATA + "\"\n"
                                                + " },\n"
                                                + "  \"log_managements\": [{\n"
                                                + "    \"destination\": \"LOG_INSIGHT\",\n"
                                                + "    \"address\": \"10.78.20.10\",\n"
                                                + "    \"port\": 9000,\n"
                                                + "    \"username\": \"foo\",\n"
                                                + "    \"password\": \"bar\"\n"
                                                + "  }]\n"
                                                + "}";

    private static final String POST_ONPREM_BODY_CR_NOTARY_VALID_CERT = "{\n"
                                                + "  \"name\": \"OnPrem\",\n"
                                                + "  \"type\": \"ON_PREM\",\n"
                                                + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                                + "  \"vcenter\": {\n"
                                                + "    \"url\": \"www.vcenter.com\",\n"
                                                + "    \"username\": \"admin\",\n"
                                                + "    \"password\": \"password\"\n"
                                                + "  },\n"
                                                + "  \"resource_pool\": \"pool\",\n"
                                                + "  \"storage\": \"datastore\",\n"
                                                + "  \"folder\": \"folder\",\n"
                                                + "  \"network\": {\n"
                                                + "    \"name\": \"Network 1\",\n"
                                                + "    \"ip_pool\": [\n"
                                                + "      \"10.1.1.16-10.1.1.64\",\n"
                                                + "      \"10.1.1.100-10.1.1.200\"\n"
                                                + "    ],\n"
                                                + "    \"gateway\": \"10.1.1.1\",\n"
                                                + "    \"subnet\": \"24\",\n"
                                                + "    \"name_servers\": [\n"
                                                + "      \"10.1.1.3\"\n"
                                                + "    ]\n"
                                                + "  },\n"
                                                + "  \"outbound_proxy\": {\n"
                                                + "    \"http_host\": \"localhost\",\n"
                                                + "    \"http_port\": 8080\n"
                                                + "  },\n"
                                                + "  \"container_repo\": {\n"
                                                + "    \"url\": \"https://docker-repo.com\",\n"
                                                + "    \"username\": \"user\",\n"
                                                + "    \"password\": \"docker\",\n"
                                                + "    \"tls_certificate_data\": \"" + VALID_TLS_CERT_DATA + "\"\n"
                                                + "  },\n"
                                                + "  \"wavefront\": {\n"
                                                + "    \"url\": \"https://wavefront.com\",\n"
                                                + "    \"token\": \"token\"\n"
                                                + "  },\n"
                                                + "  \"notary_server\": {\n"
                                                + "    \"url\": \"https://notary.test.com\",\n"
                                                + "    \"tls_certificate_data\": \"" + VALID_TLS_CERT_DATA + "\"\n"
                                                + " },\n"
                                                + "  \"log_managements\": [{\n"
                                                + "    \"destination\": \"LOG_INSIGHT\",\n"
                                                + "    \"address\": \"10.78.20.10\",\n"
                                                + "    \"port\": 9000,\n"
                                                + "    \"username\": \"foo\",\n"
                                                + "    \"password\": \"bar\"\n"
                                                + "  }]\n"
                                                + "}";

    private static final String POST_ONPREM_BODY_INVALID_CR_VALID_NOTARY_CERT = "{\n"
                                                + "  \"name\": \"OnPrem\",\n"
                                                + "  \"type\": \"ON_PREM\",\n"
                                                + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                                + "  \"vcenter\": {\n"
                                                + "    \"url\": \"www.vcenter.com\",\n"
                                                + "    \"username\": \"admin\",\n"
                                                + "    \"password\": \"password\"\n"
                                                + "  },\n"
                                                + "  \"resource_pool\": \"pool\",\n"
                                                + "  \"storage\": \"datastore\",\n"
                                                + "  \"folder\": \"folder\",\n"
                                                + "  \"network\": {\n"
                                                + "    \"name\": \"Network 1\",\n"
                                                + "    \"ip_pool\": [\n"
                                                + "      \"10.1.1.16-10.1.1.64\",\n"
                                                + "      \"10.1.1.100-10.1.1.200\"\n"
                                                + "    ],\n"
                                                + "    \"gateway\": \"10.1.1.1\",\n"
                                                + "    \"subnet\": \"24\",\n"
                                                + "    \"name_servers\": [\n"
                                                + "      \"10.1.1.3\"\n"
                                                + "    ]\n"
                                                + "  },\n"
                                                + "  \"outbound_proxy\": {\n"
                                                + "    \"http_host\": \"localhost\",\n"
                                                + "    \"http_port\": 8080\n"
                                                + "  },\n"
                                                + "  \"container_repo\": {\n"
                                                + "    \"url\": \"https://docker-repo.com\",\n"
                                                + "    \"username\": \"user\",\n"
                                                + "    \"password\": \"docker\",\n"
                                                + "    \"tls_certificate_data\": \"" + INVALID_TLS_CERT_DATA + "\"\n"
                                                + "  },\n"
                                                + "  \"wavefront\": {\n"
                                                + "    \"url\": \"https://wavefront.com\",\n"
                                                + "    \"token\": \"token\"\n"
                                                + "  },\n"
                                                + "  \"notary_server\": {\n"
                                                + "    \"url\": \"https://notary.test.com\",\n"
                                                + "    \"tls_certificate_data\": \"" + VALID_TLS_CERT_DATA + "\"\n"
                                                + " },\n"
                                                + "  \"log_managements\": [{\n"
                                                + "    \"destination\": \"LOG_INSIGHT\",\n"
                                                + "    \"address\": \"10.78.20.10\",\n"
                                                + "    \"port\": 9000,\n"
                                                + "    \"username\": \"foo\",\n"
                                                + "    \"password\": \"bar\"\n"
                                                + "  }]\n"
                                                + "}";

    private static final String POST_ONPREM_BODY_VALID_CR_INVALID_NOTARY_CERT = "{\n"
                                                + "  \"name\": \"OnPrem\",\n"
                                                + "  \"type\": \"ON_PREM\",\n"
                                                + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                                + "  \"vcenter\": {\n"
                                                + "    \"url\": \"www.vcenter.com\",\n"
                                                + "    \"username\": \"admin\",\n"
                                                + "    \"password\": \"password\"\n"
                                                + "  },\n"
                                                + "  \"resource_pool\": \"pool\",\n"
                                                + "  \"storage\": \"datastore\",\n"
                                                + "  \"folder\": \"folder\",\n"
                                                + "  \"network\": {\n"
                                                + "    \"name\": \"Network 1\",\n"
                                                + "    \"ip_pool\": [\n"
                                                + "      \"10.1.1.16-10.1.1.64\",\n"
                                                + "      \"10.1.1.100-10.1.1.200\"\n"
                                                + "    ],\n"
                                                + "    \"gateway\": \"10.1.1.1\",\n"
                                                + "    \"subnet\": \"24\",\n"
                                                + "    \"name_servers\": [\n"
                                                + "      \"10.1.1.3\"\n"
                                                + "    ]\n"
                                                + "  },\n"
                                                + "  \"outbound_proxy\": {\n"
                                                + "    \"http_host\": \"localhost\",\n"
                                                + "    \"http_port\": 8080\n"
                                                + "  },\n"
                                                + "  \"container_repo\": {\n"
                                                + "    \"url\": \"https://docker-repo.com\",\n"
                                                + "    \"username\": \"user\",\n"
                                                + "    \"password\": \"docker\",\n"
                                                + "    \"tls_certificate_data\": \"" + VALID_TLS_CERT_DATA + "\"\n"
                                                + "  },\n"
                                                + "  \"wavefront\": {\n"
                                                + "    \"url\": \"https://wavefront.com\",\n"
                                                + "    \"token\": \"token\"\n"
                                                + "  },\n"
                                                + "  \"notary_server\": {\n"
                                                + "    \"url\": \"https://notary.test.com\",\n"
                                                + "    \"tls_certificate_data\": \"" + INVALID_TLS_CERT_DATA + "\"\n"
                                                + " },\n"
                                                + "  \"log_managements\": [{\n"
                                                + "    \"destination\": \"LOG_INSIGHT\",\n"
                                                + "    \"address\": \"10.78.20.10\",\n"
                                                + "    \"port\": 9000,\n"
                                                + "    \"username\": \"foo\",\n"
                                                + "    \"password\": \"bar\"\n"
                                                + "  }]\n"
                                                + "}";

    private static final String POST_ONPREM_BODY_CR_NOTARY_INVALID_CERT = "{\n"
                                                + "  \"name\": \"OnPrem\",\n"
                                                + "  \"type\": \"ON_PREM\",\n"
                                                + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                                + "  \"vcenter\": {\n"
                                                + "    \"url\": \"www.vcenter.com\",\n"
                                                + "    \"username\": \"admin\",\n"
                                                + "    \"password\": \"password\"\n"
                                                + "  },\n"
                                                + "  \"resource_pool\": \"pool\",\n"
                                                + "  \"storage\": \"datastore\",\n"
                                                + "  \"folder\": \"folder\",\n"
                                                + "  \"network\": {\n"
                                                + "    \"name\": \"Network 1\",\n"
                                                + "    \"ip_pool\": [\n"
                                                + "      \"10.1.1.16-10.1.1.64\",\n"
                                                + "      \"10.1.1.100-10.1.1.200\"\n"
                                                + "    ],\n"
                                                + "    \"gateway\": \"10.1.1.1\",\n"
                                                + "    \"subnet\": \"24\",\n"
                                                + "    \"name_servers\": [\n"
                                                + "      \"10.1.1.3\"\n"
                                                + "    ]\n"
                                                + "  },\n"
                                                + "  \"outbound_proxy\": {\n"
                                                + "    \"http_host\": \"localhost\",\n"
                                                + "    \"http_port\": 8080\n"
                                                + "  },\n"
                                                + "  \"container_repo\": {\n"
                                                + "    \"url\": \"https://docker-repo.com\",\n"
                                                + "    \"username\": \"user\",\n"
                                                + "    \"password\": \"docker\",\n"
                                                + "    \"tls_certificate_data\": \"" + INVALID_TLS_CERT_DATA + "\"\n"
                                                + "  },\n"
                                                + "  \"wavefront\": {\n"
                                                + "    \"url\": \"https://wavefront.com\",\n"
                                                + "    \"token\": \"token\"\n"
                                                + "  },\n"
                                                + "  \"notary_server\": {\n"
                                                + "    \"url\": \"https://notary.test.com\",\n"
                                                + "    \"tls_certificate_data\": \"" + INVALID_TLS_CERT_DATA + "\"\n"
                                                + " },\n"
                                                + "  \"log_managements\": [{\n"
                                                + "    \"destination\": \"LOG_INSIGHT\",\n"
                                                + "    \"address\": \"10.78.20.10\",\n"
                                                + "    \"port\": 9000,\n"
                                                + "    \"username\": \"foo\",\n"
                                                + "    \"password\": \"bar\"\n"
                                                + "  }]\n"
                                                + "}";

    private static final String POST_MANGO_BODY = "{\n"
                                                   + "  \"name\": \"Mango\",\n"
                                                   + "  \"type\": \"Mango\",\n"
                                                   + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                                   + "  \"vcenter\": {\n"
                                                   + "    \"url\": \"http://vcenter\",\n"
                                                   + "    \"username\": \"admin\",\n"
                                                   + "    \"password\": \"password\"\n"
                                                   + "  },\n"
                                                   + "  \"resource_pool\": \"pool\",\n"
                                                   + "  \"storage\": \"datastore\",\n"
                                                   + "  \"folder\": \"folder\",\n"
                                                   + "  \"network\": {\n"
                                                   + "    \"name\": \"Network 1\",\n"
                                                   + "    \"ip_pool\": [\n"
                                                   + "      \"10.1.1.16-10.1.1.64\",\n"
                                                   + "      \"10.1.1.100-10.1.1.200\"\n"
                                                   + "    ],\n"
                                                   + "    \"gateway\": \"10.1.1.1\",\n"
                                                   + "    \"subnet\": \"24\",\n"
                                                   + "    \"name_servers\": [\n"
                                                   + "      \"10.1.1.3\"\n"
                                                   + "    ]\n"
                                                   + "  },\n"
                                                   + "  \"container_repo\": {\n"
                                                   + "    \"url\": \"https://docker-repo.com\",\n"
                                                   + "    \"username\": \"user\",\n"
                                                   + "    \"password\": \"docker\"\n"
                                                   + "  },\n"
                                                   + "  \"wavefront\": {\n"
                                                   + "    \"url\": \"https://wavefront.com\",\n"
                                                   + "    \"token\": \"token\"\n"
                                                   + "  },\n"
                                                   + "  \"log_managements\": [{\n"
                                                   + "    \"destination\": \"LOG_INSIGHT\",\n"
                                                   + "    \"address\": \"10.78.20.10\",\n"
                                                   + "    \"port\": 9000,\n"
                                                   + "    \"username\": \"foo\",\n"
                                                   + "    \"password\": \"bar\"\n"
                                                   + "  }]\n"
                                                   + "}";

    private static final String PATCH_VMC_AWS_BODY = "{\n"
                                                + "  \"name\": \"Mango\",\n"
                                                + "  \"type\": \"VMC_AWS\",\n"
                                                + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                                + "  \"vcenter\": {\n"
                                                + "    \"url\": \"http://vcenter\",\n"
                                                + "    \"username\": \"admin\",\n"
                                                + "    \"password\": \"password\"\n"
                                                + "  },\n"
                                                + "  \"resource_pool\": \"pool\",\n"
                                                + "  \"storage\": \"datastore\",\n"
                                                + "  \"folder\": \"folder\",\n"
                                                + "  \"network\": {\n"
                                                + "    \"name\": \"Network 1\",\n"
                                                + "    \"ip_pool\": [\n"
                                                + "      \"10.1.1.16-10.1.1.64\",\n"
                                                + "      \"10.1.1.100-10.1.1.200\"\n"
                                                + "    ],\n"
                                                + "    \"gateway\": \"10.1.1.1\",\n"
                                                + "    \"subnet\": \"24\",\n"
                                                + "    \"name_servers\": [\n"
                                                + "      \"10.1.1.3\"\n"
                                                + "    ]\n"
                                                + "  },\n"
                                                + "  \"container_repo\": {\n"
                                                + "    \"url\": \"https://docker-repo.com\",\n"
                                                + "    \"username\": \"user\",\n"
                                                + "    \"password\": \"docker\"\n"
                                                + "  },\n"
                                                + "  \"wavefront\": {\n"
                                                + "    \"url\": \"https://wavefront.com\",\n"
                                                + "    \"token\": \"token\"\n"
                                                + "  },\n"
                                                + "  \"log_managements\": [{\n"
                                                + "    \"destination\": \"LOG_INSIGHT\",\n"
                                                + "    \"address\": \"10.78.20.10\",\n"
                                                + "    \"port\": 9000,\n"
                                                + "    \"username\": \"foo\",\n"
                                                + "    \"password\": \"bar\"\n"
                                                + "  }]\n"
                                                + "}";

    private static final String PATCH_ONPREM_BODY = "{\n"
                                                + "  \"name\": \"OnPrem\",\n"
                                                + "  \"type\": \"ON_PREM\",\n"
                                                + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                                + "  \"vcenter\": {\n"
                                                + "    \"url\": \"http://vcenter\",\n"
                                                + "    \"username\": \"admin\",\n"
                                                + "    \"password\": \"password\"\n"
                                                + "  },\n"
                                                + "  \"resource_pool\": \"pool\",\n"
                                                + "  \"storage\": \"datastore\",\n"
                                                + "  \"folder\": \"folder\",\n"
                                                + "  \"network\": {\n"
                                                + "    \"name\": \"Network 1\",\n"
                                                + "    \"ip_pool\": [\n"
                                                + "      \"10.1.1.16-10.1.1.64\",\n"
                                                + "      \"10.1.1.100-10.1.1.200\"\n"
                                                + "    ],\n"
                                                + "    \"gateway\": \"10.1.1.1\",\n"
                                                + "    \"subnet\": \"24\",\n"
                                                + "    \"name_servers\": [\n"
                                                + "      \"10.1.1.3\"\n"
                                                + "    ]\n"
                                                + "  },\n"
                                                + "  \"container_repo\": {\n"
                                                + "    \"url\": \"https://docker-repo.com\",\n"
                                                + "    \"username\": \"user\",\n"
                                                + "    \"password\": \"docker\"\n"
                                                + "  },\n"
                                                + "  \"outbound_proxy\": {\n"
                                                + "    \"http_host\": \"HTTP_HOST\",\n"
                                                + "    \"http_port\": 8080,\n"
                                                + "    \"https_host\": \"HTTPS_HOST\",\n"
                                                + "    \"https_port\": 8080\n"
                                                + "  },\n"
                                                + "  \"wavefront\": {\n"
                                                + "    \"url\": \"https://wavefront.com\",\n"
                                                + "    \"token\": \"token\"\n"
                                                + "  },\n"
                                                + "  \"log_managements\": [{\n"
                                                + "    \"destination\": \"LOG_INSIGHT\",\n"
                                                + "    \"address\": \"10.78.20.10\",\n"
                                                + "    \"port\": 9000,\n"
                                                + "    \"username\": \"foo\",\n"
                                                + "    \"password\": \"bar\"\n"
                                                + "  }]\n"
                                                + "}";

    private static final String POST_NO_ORG_BODY = "{\n"
                                                   + "  \"name\": \"OnPrem\",\n"
                                                   + "  \"type\": \"ON_PREM\",\n"
                                                   + "  \"vcenter\": {\n"
                                                   + "    \"url\": \"http://vcenter\",\n"
                                                   + "    \"username\": \"admin\",\n"
                                                   + "    \"password\": \"password\"\n"
                                                   + "  },\n"
                                                   + "  \"resource_pool\": \"pool\",\n"
                                                   + "  \"storage\": \"datastore\",\n"
                                                   + "  \"folder\": \"folder\",\n"
                                                   + "  \"network\": {\n"
                                                   + "    \"name\": \"Network 1\",\n"
                                                   + "    \"ip_pool\": [\n"
                                                   + "      \"10.1.1.16-10.1.1.64\",\n"
                                                   + "      \"10.1.1.100-10.1.1.200\"\n"
                                                   + "    ],\n"
                                                   + "    \"gateway\": \"10.1.1.1\",\n"
                                                   + "    \"subnet\": \"24\",\n"
                                                   + "    \"name_servers\": [\n"
                                                   + "      \"10.1.1.3\"\n"
                                                   + "    ]\n"
                                                   + "  },\n"
                                                   + "  \"wavefront\": {\n"
                                                   + "    \"url\": \"https://wavefront.com\",\n"
                                                   + "    \"token\": \"token\"\n"
                                                   + "  },\n"
                                                   + "  \"container_repo\": {\n"
                                                   + "    \"url\": \"https://docker-repo.com\",\n"
                                                   + "    \"username\": \"user\",\n"
                                                   + "    \"password\": \"docker\"\n"
                                                   + "  }\n"
                                                   + "}";

    private static final String POST_NO_VCENTER_BODY = "{\n"
                                                   + "  \"name\": \"OnPrem\",\n"
                                                   + "  \"type\": \"ON_PREM\",\n"
                                                   + "  \"resource_pool\": \"pool\",\n"
                                                   + "  \"storage\": \"datastore\",\n"
                                                   + "  \"folder\": \"folder\",\n"
                                                   + "  \"network\": {\n"
                                                   + "    \"name\": \"Network 1\",\n"
                                                   + "    \"ip_pool\": [\n"
                                                   + "      \"10.1.1.16-10.1.1.64\",\n"
                                                   + "      \"10.1.1.100-10.1.1.200\"\n"
                                                   + "    ],\n"
                                                   + "    \"gateway\": \"10.1.1.1\",\n"
                                                   + "    \"subnet\": \"24\",\n"
                                                   + "    \"name_servers\": [\n"
                                                   + "      \"10.1.1.3\"\n"
                                                   + "    ]\n"
                                                   + "  },\n"
                                                   + "  \"wavefront\": {\n"
                                                   + "    \"url\": \"https://wavefront.com\",\n"
                                                   + "    \"token\": \"token\"\n"
                                                   + "  },\n"
                                                   + "  \"container_repo\": {\n"
                                                   + "    \"url\": \"https://docker-repo.com\",\n"
                                                   + "    \"username\": \"user\",\n"
                                                   + "    \"password\": \"docker\"\n"
                                                   + "  }\n"
                                                   + "}";

    private static final String POST_NO_NETWORK_BODY = "{\n"
                                                       + "  \"name\": \"OnPrem\",\n"
                                                       + "  \"type\": \"ON_PREM\",\n"
                                                       + "  \"vcenter\": {\n"
                                                       + "    \"url\": \"http://vcenter\",\n"
                                                       + "    \"username\": \"admin\",\n"
                                                       + "    \"password\": \"password\"\n"
                                                       + "  },\n"
                                                       + "  \"resource_pool\": \"pool\",\n"
                                                       + "  \"storage\": \"datastore\",\n"
                                                       + "  \"folder\": \"folder\",\n"
                                                      + "  \"wavefront\": {\n"
                                                      + "    \"url\": \"https://wavefront.com\",\n"
                                                      + "    \"token\": \"token\"\n"
                                                      + "  },\n"
                                                       + "  \"container_repo\": {\n"
                                                       + "    \"url\": \"https://docker-repo.com\",\n"
                                                       + "    \"username\": \"user\",\n"
                                                       + "    \"password\": \"docker\"\n"
                                                       + "  }\n"
                                                       + "}";

    private static final String POST_NO_FOLDER_BODY = "{\n"
                                                   + "  \"name\": \"OnPrem\",\n"
                                                   + "  \"type\": \"ON_PREM\",\n"
                                                   + "  \"vcenter\": {\n"
                                                   + "    \"url\": \"http://vcenter\",\n"
                                                   + "    \"username\": \"admin\",\n"
                                                   + "    \"password\": \"password\"\n"
                                                   + "  },\n"
                                                   + "  \"resource_pool\": \"pool\",\n"
                                                   + "  \"storage\": \"datastore\",\n"
                                                   + "  \"network\": {\n"
                                                   + "    \"name\": \"Network 1\",\n"
                                                   + "    \"ip_pool\": [\n"
                                                   + "      \"10.1.1.16-10.1.1.64\",\n"
                                                   + "      \"10.1.1.100-10.1.1.200\"\n"
                                                   + "    ],\n"
                                                   + "    \"gateway\": \"10.1.1.1\",\n"
                                                   + "    \"subnet\": \"24\",\n"
                                                   + "    \"name_servers\": [\n"
                                                   + "      \"10.1.1.3\"\n"
                                                   + "    ]\n"
                                                   + "  },\n"
                                                   + "  \"wavefront\": {\n"
                                                   + "    \"url\": \"https://wavefront.com\",\n"
                                                   + "    \"token\": \"token\"\n"
                                                   + "  },\n"
                                                   + "  \"container_repo\": {\n"
                                                   + "    \"url\": \"https://docker-repo.com\",\n"
                                                   + "    \"username\": \"user\",\n"
                                                   + "    \"password\": \"docker\"\n"
                                                   + "  }\n"
                                                   + "}";

    private static final String POST_EMPTY_FOLDER_BODY = "{\n"
                                                         + "  \"name\": \"OnPrem\",\n"
                                                         + "  \"type\": \"ON_PREM\",\n"
                                                         + "  \"vcenter\": {\n"
                                                         + "    \"url\": \"http://vcenter\",\n"
                                                         + "    \"username\": \"admin\",\n"
                                                         + "    \"password\": \"password\"\n"
                                                         + "  },\n"
                                                         + "  \"resource_pool\": \"pool\",\n"
                                                         + "  \"storage\": \"datastore\",\n"
                                                         + "  \"folder\": \"  \",\n"
                                                         + "  \"network\": {\n"
                                                         + "    \"name\": \"Network 1\",\n"
                                                         + "    \"ip_pool\": [\n"
                                                         + "      \"10.1.1.16-10.1.1.64\",\n"
                                                         + "      \"10.1.1.100-10.1.1.200\"\n"
                                                         + "    ],\n"
                                                         + "    \"gateway\": \"10.1.1.1\",\n"
                                                         + "    \"subnet\": \"24\",\n"
                                                         + "    \"name_servers\": [\n"
                                                         + "      \"10.1.1.3\"\n"
                                                         + "    ]\n"
                                                         + "  },\n"
                                                         + "  \"wavefront\": {\n"
                                                         + "    \"url\": \"https://wavefront.com\",\n"
                                                         + "    \"token\": \"token\"\n"
                                                         + "  },\n"
                                                         + "  \"container_repo\": {\n"
                                                         + "    \"url\": \"https://docker-repo.com\",\n"
                                                         + "    \"username\": \"user\",\n"
                                                         + "    \"password\": \"docker\"\n"
                                                         + "  }\n"
                                                         + "}";

    private static final String POST_NO_CONTAINER_BODY = "{\n"
                                                   + "  \"name\": \"OnPrem\",\n"
                                                   + "  \"type\": \"ON_PREM\",\n"
                                                   + "  \"vcenter\": {\n"
                                                   + "    \"url\": \"http://vcenter\",\n"
                                                   + "    \"username\": \"admin\",\n"
                                                   + "    \"password\": \"password\"\n"
                                                   + "  },\n"
                                                   + "  \"resource_pool\": \"pool\",\n"
                                                   + "  \"storage\": \"datastore\",\n"
                                                   + "  \"folder\": \"folder\",\n"
                                                   + "  \"network\": {\n"
                                                   + "    \"name\": \"Network 1\",\n"
                                                   + "    \"ip_pool\": [\n"
                                                   + "      \"10.1.1.16-10.1.1.64\",\n"
                                                   + "      \"10.1.1.100-10.1.1.200\"\n"
                                                   + "    ],\n"
                                                   + "    \"gateway\": \"10.1.1.1\",\n"
                                                   + "    \"subnet\": \"24\",\n"
                                                   + "    \"name_servers\": [\n"
                                                   + "      \"10.1.1.3\"\n"
                                                   + "    ]\n"
                                                   + "  },\n"
                                                   + "  \"wavefront\": {\n"
                                                   + "    \"url\": \"https://wavefront.com\",\n"
                                                   + "    \"token\": \"token\"\n"
                                                   + "  }\n"
                                                   + "}";

    private static final String POST_NO_CONTAINER_BODY_BUT_NOTARY = "{\n"
                                                         + "  \"name\": \"OnPrem\",\n"
                                                         + "  \"type\": \"ON_PREM\",\n"
                                                         + "  \"vcenter\": {\n"
                                                         + "    \"url\": \"http://vcenter\",\n"
                                                         + "    \"username\": \"admin\",\n"
                                                         + "    \"password\": \"password\"\n"
                                                         + "  },\n"
                                                         + "  \"resource_pool\": \"pool\",\n"
                                                         + "  \"storage\": \"datastore\",\n"
                                                         + "  \"folder\": \"folder\",\n"
                                                         + "  \"network\": {\n"
                                                         + "    \"name\": \"Network 1\",\n"
                                                         + "    \"ip_pool\": [\n"
                                                         + "      \"10.1.1.16-10.1.1.64\",\n"
                                                         + "      \"10.1.1.100-10.1.1.200\"\n"
                                                         + "    ],\n"
                                                         + "    \"gateway\": \"10.1.1.1\",\n"
                                                         + "    \"subnet\": \"24\",\n"
                                                         + "    \"name_servers\": [\n"
                                                         + "      \"10.1.1.3\"\n"
                                                         + "    ]\n"
                                                         + "  },\n"
                                                         + "  \"notary_server\": {\n"
                                                         + "    \"url\": \"https://notary.test.com\"\n"
                                                         + "  },\n"
                                                         + "  \"wavefront\": {\n"
                                                         + "    \"url\": \"https://wavefront.com\",\n"
                                                         + "    \"token\": \"token\"\n"
                                                         + "  }\n"
                                                         + "}";

    private static final String POST_ZONE_BODY = "{\n"
                                                 + "  \"name\": \"OnPrem\",\n"
                                                 + "  \"latitude\": \"45.5946\",\n"
                                                 + "  \"longitude\": \"-121.1787\",\n"
                                                 + "  \"type\": \"VMC_AWS\",\n"
                                                 + "  \"wavefront\": {\n"
                                                 + "    \"url\": \"https://wavefront.com\",\n"
                                                 + "    \"token\": \"token\"\n"
                                                 + "  }\n"
                                                 + "}";

    @Autowired
    WebApplicationContext context;

    private MockMvc mockMvc;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    private AuthenticationContext userAuth;
    private AuthenticationContext adminAuth;
    private AuthenticationContext systemAuth;

    private ObjectMapper objectMapper;

    @MockBean
    ZoneService zoneService;

    @MockBean
    ClientService clientService;

    @MockBean
    ReplicaService replicaService;

    @MockBean
    OrganizationService organizationService;

    @MockBean
    OrchestrationSiteServiceStub orchestrationClient;

    private Client c1;
    private Client c2;
    private Replica r1;
    private Replica r2;

    @BeforeEach
    void setUp() {
        mockMvc = MockMvcBuilders
                .webAppContextSetup(context)
                .apply(springSecurity())
                .build();


        userAuth = createContext("operator", ORG_ID,
                                 ImmutableList.of(VmbcRoles.ORG_USER),
                                 Collections.emptyList(),
                                 Collections.emptyList(), "");

        adminAuth = createContext("operator", ORG_ID,
                                 ImmutableList.of(VmbcRoles.ORG_USER, VmbcRoles.CONSORTIUM_ADMIN),
                                 Collections.emptyList(),
                                 Collections.emptyList(), "");

        systemAuth = createContext("operator", OPER_ORG,
                                   ImmutableList.of(VmbcRoles.ORG_USER, VmbcRoles.SYSTEM_ADMIN),
                                   Collections.emptyList(),
                                   Collections.emptyList(), "");

        // This creates our default object mapper
        objectMapper = jacksonBuilder.build();

        OnPremZone.LogManagementOnPrem logOnPrem = new OnPremZone.LogManagementOnPrem();
        logOnPrem.destination = Zone.LogDestination.LOG_INSIGHT;
        logOnPrem.address = "10.78.20.10";
        logOnPrem.port = 9000;
        logOnPrem.username = "foo";
        logOnPrem.password = "bar";

        Map<String, String> usWest =
                ImmutableMap.of("name", "US_WEST1", "geo-latitude", "45.5946", "geo-longitude", "-121.1787");
        Map<String, String> usEast =
                ImmutableMap.of("name", "US_EAST1", "geo-latitude", "33.1960", "geo-longitude", "-80.0131");

        OnPremZone ozone = getOnpremZone(OP_SITE, ORG_ID);
        ozone.setWavefront(new Wavefront("https://wavefront.com", "token"));
        ozone.setLogManagements(Lists.newArrayList(logOnPrem));
        List<Zone> sites =
                ImmutableList.of(new Zone(SITE_1, VMC_AWS, usWest),
                        new Zone(SITE_2, VMC_AWS, usEast),
                        ozone);
        when(zoneService.getAllAuthorized()).thenReturn(sites);
        when(zoneService.getAuthorized(OP_SITE)).thenReturn(ozone);
        VmcAwsZone vmcAwsZone = new VmcAwsZone();
        when(zoneService.getAuthorized(OP_SITE2)).thenReturn(vmcAwsZone);
        when(zoneService.put(any(Zone.class))).thenAnswer(i -> {
            Zone z = i.getArgument(0);
            z.setId(UUID.randomUUID());
            return z;
        });

        when(clientService.getClientsByParentId(DELETE_EMPTY_ZONE)).thenReturn(ImmutableList.of());
        when(replicaService.getReplicasByParentId(DELETE_EMPTY_ZONE)).thenReturn(ImmutableList.of());
        doNothing().when(zoneService).delete(DELETE_EMPTY_ZONE);

        c1 = new Client("publicIp", "privateIp", "password", "url", "authJwtUrl", "pass", BLOCKCHAIN_ID,
                DELETE_ZONE_WITH_CLIENT, CLIENT_GROUP_ID, CLIENT_GROUP_NAME, "pem", "crt", "cacrt");
        c1.setId(CLIENT_ID_1);

        r1 = new Replica("publicIp", "privateIp", "hostname", "url", "cert",
                DELETE_ZONE_WITH_REPLICA, Replica.ReplicaType.DAML_PARTICIPANT, BLOCKCHAIN_ID, "password");
        r1.setId(REPLICA_ID_1);

        c2 = new Client("publicIp", "privateIp", "password", "url", "authJwtUrl", "pass", BLOCKCHAIN_ID,
                DELETE_ZONE_WITH_REPLICA_AND_CLIENT, CLIENT_GROUP_ID, CLIENT_GROUP_NAME, "pem", "crt", "cacrt");
        c2.setId(CLIENT_ID_2);

        r2 = new Replica("publicIp", "privateIp", "hostname", "url", "cert",
                DELETE_ZONE_WITH_REPLICA_AND_CLIENT, Replica.ReplicaType.DAML_PARTICIPANT, BLOCKCHAIN_ID, "password");
        r2.setId(REPLICA_ID_2);

        when(clientService.getClientsByParentId(DELETE_ZONE_WITH_CLIENT)).thenReturn(ImmutableList.of(c1));
        when(replicaService.getReplicasByParentId(DELETE_ZONE_WITH_CLIENT)).thenReturn(ImmutableList.of());

        when(clientService.getClientsByParentId(DELETE_ZONE_WITH_REPLICA)).thenReturn(ImmutableList.of());
        when(replicaService.getReplicasByParentId(DELETE_ZONE_WITH_REPLICA)).thenReturn(ImmutableList.of(r1));

        when(clientService.getClientsByParentId(DELETE_ZONE_WITH_REPLICA_AND_CLIENT)).thenReturn(ImmutableList.of(c2));
        when(replicaService.getReplicasByParentId(DELETE_ZONE_WITH_REPLICA_AND_CLIENT))
                .thenReturn(ImmutableList.of(r2));

        // Test Zone dependencies
        OnPremZone zoneWithDependencies = getOnpremZone(ZONE_WITH_DEPENDENCIES, ORG_ID);
        when(zoneService.getAuthorized(ZONE_WITH_DEPENDENCIES)).thenReturn(zoneWithDependencies);
        when(clientService.getClientsByParentId(ZONE_WITH_DEPENDENCIES)).thenReturn(ImmutableList.of(c2));
        when(replicaService.getReplicasByParentId(ZONE_WITH_DEPENDENCIES))
                .thenReturn(ImmutableList.of(r2));


        // Mock DELETE test Zones.
        when(zoneService.getAuthorized(DELETE_ZONE_WITH_CLIENT)).thenReturn(new Zone());
        when(zoneService.getAuthorized(DELETE_ZONE_WITH_REPLICA)).thenReturn(new Zone());
        when(zoneService.getAuthorized(DELETE_ZONE_WITH_REPLICA_AND_CLIENT)).thenReturn(new Zone());
        when(zoneService.getAuthorized(DELETE_EMPTY_ZONE)).thenReturn(new Zone());


        // Mock unavailable Zone.
        when(zoneService.getAuthorized(MISSING_ZONE)).thenReturn(null);
        when(zoneService.getAuthorized(NOT_FOUND_ZONE)).thenThrow(new NotFoundException("Not Found"));

        ReflectionTestUtils.setField(BlockchainUtils.class,
                                     "wavefrontEndpoint",
                                     "https://vmware.wavefront.com");

        ReflectionTestUtils.setField(BlockchainUtils.class,
                                     "wavefrontToken",
                                     "token");
    }

    @Test
    void testGet() throws Exception {
        MvcResult result =  mockMvc.perform(get("/api/blockchains/zones").with(authentication(userAuth)))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<ZoneListResponse> zones =
                objectMapper.readValue(body, new TypeReference<List<ZoneListResponse>>() {});
        Assertions.assertEquals(3, zones.size());
        Assertions.assertEquals("US_WEST1", zones.get(0).getName());
        Assertions.assertEquals("45.5946", zones.get(0).getLatitude());
        Assertions.assertEquals("-121.1787", zones.get(0).getLongitude());
        Assertions.assertEquals(VMC_AWS, zones.get(0).getType());
        Assertions.assertEquals("US_EAST1", zones.get(1).getName());
        Assertions.assertEquals("33.1960", zones.get(1).getLatitude());
        Assertions.assertEquals("-80.0131", zones.get(1).getLongitude());
        Assertions.assertEquals(VMC_AWS, zones.get(1).getType());
        // Make sure the on-prem site returns the right type
        Assertions.assertEquals(ON_PREM, zones.get(2).getType());
    }

    @Test
    void testGetDetails() throws Exception {
        MvcResult result =  mockMvc.perform(get("/api/blockchains/zones/" + OP_SITE)
                .with(authentication(adminAuth)))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        ZoneResponse z = objectMapper.readValue(body, ZoneResponse.class);
        Assertions.assertTrue(z instanceof OnPremGetResponse);
        OnPremGetResponse r = (OnPremGetResponse) z;
        Assertions.assertEquals("On Prem", r.getName());
        Assertions.assertNotNull(r.getContainerRepo());
        Assertions.assertNotNull(r.getVcenter());
        Assertions.assertNotNull(r.getName());
        Assertions.assertEquals("Network 1", r.getNetwork().getName());
        Assertions.assertEquals("admin", r.getVcenter().getUsername());
        Assertions.assertEquals("https://wavefront.com", r.getWavefront().getUrl());
        Assertions.assertEquals(1, r.logManagements.size());
        Assertions.assertEquals(Zone.LogDestination.LOG_INSIGHT, r.logManagements.get(0).destination);
        Assertions.assertEquals(new Integer(9000), r.getLogManagements().get(0).port);
    }

    @Test
    void testUnavailableZoneGet() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/blockchains/zones/" + MISSING_ZONE)
                .with(authentication(adminAuth)))
                .andExpect(status().isNotFound()).andReturn();

        String body = result.getResponse().getContentAsString();

        String message = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(MessageFormat.format(ErrorCode.ZONE_NOT_FOUND, MISSING_ZONE.toString()), message);
    }

    @Test
    void testNotFoundZoneGet() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/blockchains/zones/" + NOT_FOUND_ZONE)
                .with(authentication(adminAuth)))
                .andExpect(status().isNotFound()).andReturn();

        String body = result.getResponse().getContentAsString();

        String message = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(MessageFormat.format(ErrorCode.ZONE_NOT_FOUND, NOT_FOUND_ZONE.toString()), message);
    }

    @Test
    void testGetDependencies() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/blockchains/zones/dependencies/" + ZONE_WITH_DEPENDENCIES)
                .with(authentication(adminAuth)))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();

        DependentNodesGetResponse response = objectMapper.readValue(body, DependentNodesGetResponse.class);

        Assertions.assertEquals(ImmutableList.of(REPLICA_ID_2), response.getReplicaList());
        Assertions.assertEquals(ImmutableList.of(CLIENT_ID_2), response.getClientList());
    }

    @Test
    void testGetDependenciesZoneNotFound() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/blockchains/zones/dependencies/" + NOT_FOUND_ZONE)
                .with(authentication(adminAuth)))
                .andExpect(status().isNotFound()).andReturn();

        String body = result.getResponse().getContentAsString();

        String message = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(MessageFormat.format(ErrorCode.ZONE_NOT_FOUND, NOT_FOUND_ZONE.toString()), message);
    }


    @Test
    void testLoadZonesBadAction() throws Exception {
        mockMvc.perform(post("/api/blockchains/zones?action=sleep").with(authentication(adminAuth)))
                .andExpect(status().isBadRequest());

    }

    @Test
    void testPostOnPrem() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones").with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_ONPREM_BODY))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        ZoneResponse zone = objectMapper.readValue(body, OnPremGetResponse.class);
        verify(zoneService, times(1)).put(any(Zone.class));
        Assertions.assertTrue(zone instanceof OnPremGetResponse);
        // This was done as a consortium admin.  The org id should have been changed.
        Assertions.assertEquals(UUID.fromString("9ecb07bc-482c-48f3-80d0-23c4f9514902"),
                                ((OnPremGetResponse) zone).getOrgId());
        Assertions.assertEquals("admin", ((OnPremGetResponse) zone).getVcenter().getUsername());
        Assertions.assertNotNull(((OnPremGetResponse) zone).getOutboundProxy());
        Assertions.assertEquals("localhost", ((OnPremGetResponse) zone).getOutboundProxy().getHttpHost());
    }

    @Test
    void testPostOnPremBadRequest() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones").with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON)
                .content(POST_ONPREM_BODY_BAD_REQUEST))
                .andExpect(status().isBadRequest()).andReturn();
    }

    @Test
    void testPostOnPremBadVcenter() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones").with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON)
                .content(POST_ONPREM_BODY_BAD_VCENTER))
                .andExpect(status().isBadRequest()).andReturn();
    }

    @Test
    void testPostOnPremMissingName() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones").with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON)
                .content(POST_ONPREM_BODY_MISSING_NAME))
                .andExpect(status().isBadRequest()).andReturn();
    }

    @Test
    void testPostOnPremBadNetwork() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones").with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON)
                .content(POST_ONPREM_BODY_BAD_NETWORK))
                .andExpect(status().isBadRequest()).andReturn();
    }

    @Test
    void testPostOnPremMissingResourcePool() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones").with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON)
                .content(POST_ONPREM_BODY_MISSING_RESOURCE_POOL))
                .andExpect(status().isBadRequest()).andReturn();
    }

    @Test
    void testPostMango() throws Exception {
        mockMvc.perform(post("/api/blockchains/zones").with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_MANGO_BODY))
                .andExpect(status().isBadRequest());
    }

    @Test
    void testPostOnPremNoOrg() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones").with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_NO_ORG_BODY))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        ZoneResponse zone = objectMapper.readValue(body, ZoneResponse.class);
        verify(zoneService, times(1)).put(any(Zone.class));
        Assertions.assertTrue(zone instanceof OnPremGetResponse);
        // Org not specified, so it should be filled in
        Assertions.assertEquals(UUID.fromString("9ecb07bc-482c-48f3-80d0-23c4f9514902"),
                                ((OnPremGetResponse) zone).getOrgId());
        Assertions.assertEquals("admin", ((OnPremGetResponse) zone).getVcenter().getUsername());
    }

    @Test
    void testPostOnPremSysadmin() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones").with(authentication(systemAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_ONPREM_BODY))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        ZoneResponse zone = objectMapper.readValue(body, ZoneResponse.class);
        // This was done as a system admin.  The org_id should not have been changed.
        Assertions.assertEquals(UUID.fromString("5e5ff1c8-34b9-4fa3-9924-83eb14354d4c"),
                                ((OnPremGetResponse) zone).getOrgId());
        Assertions.assertEquals("admin", ((OnPremGetResponse) zone).getVcenter().getUsername());
    }

    @Test
    void testPostOnPremSysadminNoOrg() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones").with(authentication(systemAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_NO_ORG_BODY))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        ZoneResponse zone = objectMapper.readValue(body, ZoneResponse.class);
        // No org, so it should be filled in from OPER_ORG
        Assertions.assertEquals(OPER_ORG,
                                ((OnPremGetResponse) zone).getOrgId());
        Assertions.assertEquals("admin", ((OnPremGetResponse) zone).getVcenter().getUsername());
    }

    @Test
    void testSiteTest() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_ONPREM_BODY))
                .andExpect(status().isOk()).andReturn();
    }

    @Test
    void testSiteNoContainer() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_NO_CONTAINER_BODY))
                .andExpect(status().isOk()).andReturn();
    }

    @Test
    void testOnPremNoNotaryUrlButCert() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_ONPREM_BODY_NO_NOTARY_URL_BUT_CERT))
                .andExpect(status().isBadRequest()).andReturn();
    }

    @Test
    void testSiteNoContainerButNotary() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_NO_CONTAINER_BODY_BUT_NOTARY))
                .andExpect(status().isBadRequest()).andReturn();
    }

    @Test
    void testSiteContainerRegWithValidCert() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_ONPREM_BODY_CR_VALID_CERT))
                .andExpect(status().isOk()).andReturn();
    }

    @Test
    void testSiteContainerRegWithInvalidCert() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_ONPREM_BODY_CR_INVALID_CERT))
                .andExpect(status().isBadRequest()).andReturn();
    }

    @Test
    void testSiteNotaryWithValidCert() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_ONPREM_BODY_NOTARY_VALID_CERT))
                .andExpect(status().isOk()).andReturn();
    }

    @Test
    void testSiteNotaryInvalidCert() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_ONPREM_BODY_NOTARY_INVALID_CERT))
                .andExpect(status().isBadRequest()).andReturn();
    }

    @Test
    void testSiteContainerRegNotaryWithValidCert() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_ONPREM_BODY_CR_NOTARY_VALID_CERT))
                .andExpect(status().isOk()).andReturn();
    }

    @Test
    void testSiteValidContainerRegInvalidNotaryWithCert() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_ONPREM_BODY_VALID_CR_INVALID_NOTARY_CERT))
                .andExpect(status().isBadRequest()).andReturn();
    }

    @Test
    void testSiteInvalidContainerRegValidNotaryWithCert() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_ONPREM_BODY_INVALID_CR_VALID_NOTARY_CERT))
                .andExpect(status().isBadRequest()).andReturn();
    }

    @Test
    void testSiteContainerRegNotaryWithInvalidCert() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_ONPREM_BODY_CR_NOTARY_INVALID_CERT))
                .andExpect(status().isBadRequest()).andReturn();
    }

    @Test
    void testSiteNoVcenter() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_NO_VCENTER_BODY))
                .andExpect(status().isBadRequest());
    }

    @Test
    void testSiteNoNetwork() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        mockMvc.perform(post("/api/blockchains/zones?action=test")
                                .with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_NO_NETWORK_BODY))
                .andExpect(status().isBadRequest());
    }

    @Test
    void testSiteNoFolder() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        mockMvc.perform(post("/api/blockchains/zones?action=test")
                                .with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_NO_FOLDER_BODY))
                .andExpect(status().isBadRequest());
    }

    @Test
    void testSiteEmptyFolder() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        mockMvc.perform(post("/api/blockchains/zones?action=test")
                                .with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_EMPTY_FOLDER_BODY))
                .andExpect(status().isBadRequest());
    }


    @Test
    void testSiteTestError() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onError(new BadRequestException("bad"));
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_ONPREM_BODY))

                .andExpect(status().is5xxServerError()).andReturn();
    }

    @Test
    void testUtils() throws Exception {
        Zone op = getOnpremZone(OP_SITE, ORG_ID);
        OrchestrationSiteInfo info = BlockchainUtils.toInfo(op, new Organization());
        System.out.println(info);
    }

    @Test
    void testPatch() throws Exception {
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                        any(StreamObserver.class));

        MvcResult result = mockMvc.perform(patch("/api/blockchains/zones/" + OP_SITE)
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON)
                .content(PATCH_ONPREM_BODY))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();
        ZoneResponse zone = objectMapper.readValue(body, ZoneResponse.class);
        // verify(zoneService, times(1)).put(any(Zone.class));
        Assertions.assertTrue(zone instanceof OnPremGetResponse);
        // ORG ID for PATCH_ONPREM_BODY
        Assertions.assertEquals(UUID.fromString("5e5ff1c8-34b9-4fa3-9924-83eb14354d4c"),
                ((OnPremGetResponse) zone).getOrgId());
        Assertions.assertEquals("admin", ((OnPremGetResponse) zone).getVcenter().getUsername());
    }

    @Test
    void testUnavailableZonePatch() throws Exception {
        MvcResult result = mockMvc.perform(patch("/api/blockchains/zones/" + MISSING_ZONE)
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON)
                .content(PATCH_ONPREM_BODY))
                .andExpect(status().isNotFound()).andReturn();

        String body = result.getResponse().getContentAsString();

        String message = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(MessageFormat.format(ErrorCode.ZONE_NOT_FOUND, MISSING_ZONE.toString()), message);
    }

    @Test
    void testNotFoundZonePatch() throws Exception {
        MvcResult result = mockMvc.perform(patch("/api/blockchains/zones/" + NOT_FOUND_ZONE)
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON)
                .content(PATCH_ONPREM_BODY))
                .andExpect(status().isNotFound()).andReturn();

        String body = result.getResponse().getContentAsString();

        String message = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(MessageFormat.format(ErrorCode.ZONE_NOT_FOUND, NOT_FOUND_ZONE.toString()), message);
    }

    @Test
    void testDelete() throws Exception {
        MvcResult result = mockMvc.perform(delete("/api/blockchains/zones/" + DELETE_EMPTY_ZONE)
                .with(authentication(adminAuth)))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();

        String id = objectMapper.readValue(body, Map.class).get("id").toString();

        Assertions.assertEquals(DELETE_EMPTY_ZONE.toString(), id);
    }

    @Test
    void testDeleteBadAuth() throws Exception {
        mockMvc.perform(delete("/api/blockchains/zones/" + DELETE_EMPTY_ZONE)
                .with(authentication(userAuth)))
                .andExpect(status().isForbidden());
    }

    @Test
    void testUnavailableZoneDelete() throws Exception {
        MvcResult result = mockMvc.perform(delete("/api/blockchains/zones/" + MISSING_ZONE)
                .with(authentication(adminAuth)))
                .andExpect(status().isNotFound()).andReturn();

        String body = result.getResponse().getContentAsString();

        String message = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(MessageFormat.format(ErrorCode.ZONE_NOT_FOUND, MISSING_ZONE.toString()), message);
    }

    @Test
    void testNotFoundZoneDelete() throws Exception {
        MvcResult result = mockMvc.perform(delete("/api/blockchains/zones/" + NOT_FOUND_ZONE)
                .with(authentication(adminAuth)))
                .andExpect(status().isNotFound()).andReturn();

        String body = result.getResponse().getContentAsString();

        String message = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(MessageFormat.format(ErrorCode.ZONE_NOT_FOUND, NOT_FOUND_ZONE.toString()), message);
    }

    @Test
    void testDeleteZoneWithClients() throws Exception {
        MvcResult result = mockMvc.perform(delete("/api/blockchains/zones/" + DELETE_ZONE_WITH_CLIENT)
                .with(authentication(adminAuth)))
                .andExpect(status().isBadRequest()).andReturn();

        String body = result.getResponse().getContentAsString();

        String errorMessage = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(
                String.format("Zone %s has following dependencies. Clients: %s Replicas %s Cannot be deleted.",
                        DELETE_ZONE_WITH_CLIENT.toString(), ImmutableList.of(CLIENT_ID_1), Collections.emptyList()),
                errorMessage);
    }

    @Test
    void testDeleteZoneWithReplicas() throws Exception {
        MvcResult result = mockMvc.perform(delete("/api/blockchains/zones/" + DELETE_ZONE_WITH_REPLICA)
                .with(authentication(adminAuth)))
                .andExpect(status().isBadRequest()).andReturn();

        String body = result.getResponse().getContentAsString();

        String errorMessage = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(
                String.format("Zone %s has following dependencies. Clients: %s Replicas %s Cannot be deleted.",
                        DELETE_ZONE_WITH_REPLICA.toString(), Collections.emptyList(), ImmutableList.of(REPLICA_ID_1)),
                errorMessage);
    }

    @Test
    void testDeleteZoneWithReplicasAndClients() throws Exception {
        MvcResult result = mockMvc.perform(delete("/api/blockchains/zones/" + DELETE_ZONE_WITH_REPLICA_AND_CLIENT)
                .with(authentication(adminAuth)))
                .andExpect(status().isBadRequest()).andReturn();

        String body = result.getResponse().getContentAsString();

        String errorMessage = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(
                String.format(
                        "Zone %s has following dependencies. Clients: %s Replicas %s Cannot be deleted.",
                        DELETE_ZONE_WITH_REPLICA_AND_CLIENT.toString(),
                        ImmutableList.of(CLIENT_ID_2),
                        ImmutableList.of(REPLICA_ID_2)
                ),
                errorMessage);
    }



    @Test
    void testPatchVmcAws() throws Exception {
        // We do not support PATCH for VMC_AWS zones as of now
        // This might change in the future, change this test when that happens
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                        any(StreamObserver.class));

        MvcResult result = mockMvc.perform(patch("/api/blockchains/zones/" + OP_SITE2)
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON)
                .content(PATCH_VMC_AWS_BODY))
                .andExpect(status().isBadRequest()).andReturn();
    }

}
