/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import static com.vmware.blockchain.security.MvcTestSecurityConfig.createContext;
import static com.vmware.blockchain.services.blockchains.BlockchainApiObjects.BlockchainPatch;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.authentication;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.deployment.v1.DeployedResource;
import com.vmware.blockchain.deployment.v1.DeploymentExecutionEvent;
import com.vmware.blockchain.deployment.v1.DeploymentExecutionEvent.Status;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeploymentRequestResponse;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceV2Grpc;
import com.vmware.blockchain.deployment.v1.StreamDeploymentSessionEventRequest;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.security.MvcTestSecurityConfig;
import com.vmware.blockchain.security.SecurityTestUtils;
import com.vmware.blockchain.services.blockchains.Blockchain.BlockchainType;
import com.vmware.blockchain.services.blockchains.BlockchainApiObjects.BlockchainTaskResponse;
import com.vmware.blockchain.services.blockchains.clients.Client;
import com.vmware.blockchain.services.blockchains.clients.ClientService;
import com.vmware.blockchain.services.blockchains.nodesizing.NodeSizeTemplate;
import com.vmware.blockchain.services.blockchains.nodesizing.NodeSizeTemplateService;
import com.vmware.blockchain.services.blockchains.nodesizing.NodeSizeTemplateUtil;
import com.vmware.blockchain.services.blockchains.zones.VmcAwsZone;
import com.vmware.blockchain.services.blockchains.zones.Zone;
import com.vmware.blockchain.services.blockchains.zones.ZoneService;
import com.vmware.blockchain.services.blockchains.zones.ZoneTestUtils;
import com.vmware.blockchain.services.concord.ConcordService;
import com.vmware.blockchain.services.profiles.Consortium;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.VmbcRoles;
import com.vmware.blockchain.services.tasks.Task;
import com.vmware.blockchain.services.tasks.Task.State;
import com.vmware.blockchain.services.tasks.TaskController;
import com.vmware.blockchain.services.tasks.TaskService;

import io.grpc.stub.StreamObserver;

/**
 * Tests for the blockchain controller.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:lint-test.properties")
@WebMvcTest(controllers = { BlockchainController.class, TaskController.class })
@ContextConfiguration(classes = {MvcTestSecurityConfig.class, MvcConfig.class})
@ComponentScan(basePackageClasses = { BlockchainControllerTest.class, HelenExceptionHandler.class,
                                      TaskController.class})
public class BlockchainControllerTest {
    static final UUID BC_ID = UUID.fromString("437d97b2-76df-4596-b0d8-3d8a9412ff2f");
    static final UUID BC2_ID = UUID.fromString("7324cb8f-0ffc-4311-b57e-4c3e1e10a3aa");
    static final UUID BC_NEW = UUID.fromString("4b8a5ec6-91ad-437d-b574-45f5b7345b96");
    static final UUID BC_MISSING = UUID.fromString("d3cad1e0-b520-47f5-9dfd-4cb28d59dfe8");
    static final UUID BC_UNAVAILABLE = UUID.fromString("a15cfd74-5f4b-4af4-a286-888a036d7889");

    static final UUID BC_DAML = UUID.fromString("fd7167b0-057d-11ea-8d71-362b9e155667");

    static final UUID BC_DEREGISTER = UUID.fromString("691f2038-7545-11ea-bc55-0242ac130003");
    static final UUID BC_DEREGISTER_INACTIVE = UUID.fromString("8a304abc-4964-488e-a3c6-1379e6a67229");

    static final UUID C2_ID = UUID.fromString("04e4f62d-5364-4363-a582-b397075b65a3");
    static final UUID C3_ID = UUID.fromString("a4b8f7ed-00b3-451e-97bc-4aa51a211288");

    static final UUID TASK_ID = UUID.fromString("c23ed97d-f29c-472e-9f63-cc6be883a5f5");

    static final UUID ORG_ID = UUID.fromString("5c373085-0cd1-47e4-b4f2-66d418f22fdf");
    static final UUID ORG2_ID = UUID.fromString("a774d0e3-b182-4330-93df-6738c8b1b2de");

    static final UUID DEP_ID = UUID.fromString("67376aed-333c-4e35-b6b6-c59800752dc3");

    private static final UUID SITE_1 = UUID.fromString("84b9a0ed-c162-446a-b8c0-2e45755f3844");
    private static final UUID SITE_2 = UUID.fromString("275638a3-8860-4925-85de-c73d45cb7232");
    private static final UUID NODE_1 = UUID.fromString("f81899ce-861f-4479-9adf-f3ad753fcaf6");

    private static final String pem = "-----BEGIN PRIVATE KEY-----\\n"
            + "MIIJQwIBADANBgkqhkiG9w0BAQEFAASCCS0wggkpAgEAAoICAQDeojrK2KNwJWoH\\n"
            + "udDB8/k17H5zJns0mlbXP7INfULuwxwGS0xF5MhB3tYHINlJ5cPDDMS4YzXh0AR8\\n"
            + "pKj5wKNlt2oqVE67HYmMm73ZCrV0duORnZQMyrREiaBW5eUqbPCxYcJmoNzG9Rbz\\n"
            + "kCcG+LVkDFPDTbOQ6UgHKm1vx6Eq9uT4mneI7q8rgrJ5/7Oia953DNd84Nx/4EbO\\n"
            + "hMDT3RqU9OnY9yiPTXgsVVcVXNyY0mTME4geVKO4RbyE/zTlDe/f5L/fJw83RutX\\n"
            + "iAQ1rd0DDspDn9fsecw5bsHIefOfp/p3sJm2pvTLn94ObaDMnJey/AiUSD0N2UuQ\\n"
            + "igXmTWoaPvlQMQe/etxvE8cQIB1rLuXVBnmfXue7KK8zY1YlIt3eo5J3+p7RscXC\\n"
            + "GnN0QnkdQtfY58wolvWlHu07D/TNS5oHiOPed+Ep/XlRs2lSBIqwcYbLRTdmXX7Q\\n"
            + "koetglIpHraAamctIub0fFvZRQORAwVAEFJS4WDciJSv0NbMgO4GpcIWjpE4jrWO\\n"
            + "u6AUDo+0NtPHef/Urm69FiZuOMkyeYRHichoKkW4i56fRrujp10ARbZljcH2K6hd\\n"
            + "xZwbSkIfbZyAHuqx5wI4ua4VJrDlSrqHUQ7kkSotMsjvMFjejsJ6owcKzHUnh7Oe\\n"
            + "Fbhu3QqwetgLGKaCk+h8rhRGNzmHJQIDAQABAoICAC5Kt0TUVO+NqAVhOqoJi8kN\\n"
            + "mqFg3/9pFCN/qAsr/43b3ejlIT/rOUSRDBnBV80IzHKzJfhdEKgac8RjDIuZ5Z/R\\n"
            + "ym5wx0oa8y3ceFY35tEHo3v0swMR5vfVUhSfis4OhuGrPRvP2Pg5oAMW9zZ44mai\\n"
            + "NYyHjI3WI9bxmH3EmZtwaN3UaIR9dMjsTSYW6Ageu8wvTGM38kFYpgBtFBc5gzxz\\n"
            + "zGgDxFg/uHwnZosXmvrfzHuumAAOg1hej7trwVjbVARO8SJ6YayRv3+c1Zs/wpPP\\n"
            + "MHltNB2EmdVbVUnVKbYJ3IekneGfx4jLfgWr/+JrsRNTWolSl/dYNmEggeeIM0ZM\\n"
            + "cuSOgPDFMDLcRn5FPuOWBViSkBIJiipuGWfocTXlbNB6jqHoV2nblLCqu2sSFaEC\\n"
            + "UuJNf7yxeXc+i70C7GzCpmRc21zd6luckLhBnrEOl+VT8IFjrEibik9qqhzeZCXc\\n"
            + "Iy7mNMwidKglri/FCfCwVSfJp27Hk2njSynsD53AdLNYm1MjRzVqHLIrO8sfDr8C\\n"
            + "6hIW+HDaiJW3/J8JuufssrRp1QlSm1y8EqcJlOTdDflNFKgRxW9kK1DfR75XzjD0\\n"
            + "Xqwp/jyobDQtABHFn5R0i1/UDthjrf+1n9D5nWbSS73T1AC4cd24VtMGY1nm7lel\\n"
            + "Q6Wej5cyAm2VwREQ18RlAoIBAQD4pPqrdHx4KeEGbK8kfI4Bgdb2IaoCWgY/3hH7\\n"
            + "rznQWTi4skEwO3FbTXp/57WmMlWHAnEOH1ck1oq6C2B6eczsB6QZGSem6Ewgrfcm\\n"
            + "6+ELqN9+NGkgfvl2yNDAxKg+HkJ3zuDy1v31uaHbW9zGHSZ8s1aazkw5N+gCfIuS\\n"
            + "7p307qxj5NUjcmT7gMCy4x0RxVYwu4RXpoLiWiAc8y5nB7dGItgLeP43s9/MWh30\\n"
            + "kq+4Y8OsewIgeAH4kH0woUqp2hUP+oq6mC8WmYaXow8/rXBIqlz+lldUqRygOWhP\\n"
            + "Ml9s6kpE8XGz5MReUnwWsuggqlEBdn+kRwHc193F79/o4TcXAoIBAQDlOERwCXDY\\n"
            + "tfkbIq+qIXV6n0B5km3ARF2kdPcQRmhyDAE2K0GcqA4XfPPsAFq7v8ME7s/JjkSj\\n"
            + "lSk8J212Bi0CDRTBWRmlO8rG8JKU7Q5u3Etm+RYTGuXGcA8X0xKh+7m1vr5OhWA0\\n"
            + "K2BfxJNmukYJZPpLiws9lQE/YceQq0L3VWDiD4hEzwwPJ9/msr0I5C2jCI6PZJ0E\\n"
            + "iy/omwYjlvSpQJ0nE8iPzDC3A18IGx4qeP7FpLhQWT4rHVfh0pnHfZLzLZYrtjwg\\n"
            + "vdLkcDqQ5AuFYdJzaMgaP+NS0+9hE9o7eiSKT3MG0Zwsi0hCfqK9pjplw0EOpEPW\\n"
            + "ICogztm6+FkjAoIBAQDUHMnLImBckj1pIaZ31dm+52jeJ/HEd9AFBAkLUHxdhP+i\\n"
            + "cE60OsGFRh9OpfiMgzv7JNYVWh8ZSfymobX4RZfPcuVGk/MDq//lkQLarTcan0Gp\\n"
            + "hER6eFeQQdfz2tA7zcyeMFouT8HusiGl5EP+rjd7AfLLtVnJJixCksTu36jYnhNq\\n"
            + "VHiO+LO06i15bc4KrvHMOOu5ak8VkhT7HWlkc2yh5G+xudIE6unshqQvVEObZWAz\\n"
            + "7+vsjvP/fCOQLMhpBaYaL33RmKny+Z2cFnqXs/mpw8v5U3/EtU+6T77wcOR/IY+i\\n"
            + "KOb6tBng7TT2mED2PNVGDjTti7XHFFAY332ASUCZAoIBAAklaJ2r0sPi5vCtPqMk\\n"
            + "OKLO7eyr6hMs5yujuyP8GzjJv9agfTq5/e3z/2ugS/6H1UlCGippsdVxIzcSP6zg\\n"
            + "7DTTvstEOeC89QdhHAgzOUs1IREXyUiz3w0+9Ws28dxixfM5jDBn5AHQ5USM/HCx\\n"
            + "6A+B5vbSsn0fj/Auf58HJmHmFzqN8hsEw+9q4OU8poxPRn7l3YJdOmKvBxdZ8DHF\\n"
            + "WTfTqzGe+Xa7aAo1aTpSkq6RK2FJhE04cmGYBySsmZQ7L3ziicZuvBF2YY7Z54xW\\n"
            + "fRsyXwpZ2PorKG/qoveVqjSLWB/osadI/9lLKXP1x5qXombjFpp19Xr6x5ONjWSH\\n"
            + "ek8CggEBAOD7rePnEhFajNpXFTRUzHe6LWwmV4AUkdj8x7+vG6Oscp/sByVU8+FA\\n"
            + "mwck+su0jCox+H5rHdPlzBtfV1cVbPv5BQKZePmpruKtyf3PklwfJNB1GscOoD6c\\n"
            + "obTQIxSsPtcbM8g5V4ei7RwWZtXtXX+aQc+OgNw+y1jKbSIipdkp0uVOw6dEDyGK\\n"
            + "BE/W46kwBgtE86JzPGRar9PlhrS5q1q/03CO0LBmrzpe7MW6kjOKLZkM8QnLQ7gF\\n"
            + "6c3YT+CSXZuGDdBqqx3FJW6ABTKhaH8nb8m2h03LEtqpG1vCmMKWEhJHqscf1iNS\\n"
            + "LfIMSooMNhY1dT8pgYQM5F8tFXFBgUQ=\\n"
            + "-----END PRIVATE KEY-----\\n";

    private static final String crt = "-----BEGIN CERTIFICATE-----\\n"
            + "MIIEqjCCApICCQCeXPJRYX+VazANBgkqhkiG9w0BAQsFADAVMRMwEQYDVQQDDAow\\n"
            + "LjAuMC4wLmNhMB4XDTIwMTExMjIxMTMwM1oXDTMwMTExMDIxMTMwM1owGTEXMBUG\\n"
            + "A1UEAwwOMC4wLjAuMC5jbGllbnQwggIiMA0GCSqGSIb3DQEBAQUAA4ICDwAwggIK\\n"
            + "AoICAQDeojrK2KNwJWoHudDB8/k17H5zJns0mlbXP7INfULuwxwGS0xF5MhB3tYH\\n"
            + "INlJ5cPDDMS4YzXh0AR8pKj5wKNlt2oqVE67HYmMm73ZCrV0duORnZQMyrREiaBW\\n"
            + "5eUqbPCxYcJmoNzG9RbzkCcG+LVkDFPDTbOQ6UgHKm1vx6Eq9uT4mneI7q8rgrJ5\\n"
            + "/7Oia953DNd84Nx/4EbOhMDT3RqU9OnY9yiPTXgsVVcVXNyY0mTME4geVKO4RbyE\\n"
            + "/zTlDe/f5L/fJw83RutXiAQ1rd0DDspDn9fsecw5bsHIefOfp/p3sJm2pvTLn94O\\n"
            + "baDMnJey/AiUSD0N2UuQigXmTWoaPvlQMQe/etxvE8cQIB1rLuXVBnmfXue7KK8z\\n"
            + "Y1YlIt3eo5J3+p7RscXCGnN0QnkdQtfY58wolvWlHu07D/TNS5oHiOPed+Ep/XlR\\n"
            + "s2lSBIqwcYbLRTdmXX7QkoetglIpHraAamctIub0fFvZRQORAwVAEFJS4WDciJSv\\n"
            + "0NbMgO4GpcIWjpE4jrWOu6AUDo+0NtPHef/Urm69FiZuOMkyeYRHichoKkW4i56f\\n"
            + "Rrujp10ARbZljcH2K6hdxZwbSkIfbZyAHuqx5wI4ua4VJrDlSrqHUQ7kkSotMsjv\\n"
            + "MFjejsJ6owcKzHUnh7OeFbhu3QqwetgLGKaCk+h8rhRGNzmHJQIDAQABMA0GCSqG\\n"
            + "SIb3DQEBCwUAA4ICAQBkyUtZuZisSm4oCX5IGjTiYQne7Qx/RiFAecBkc9zcqeFy\\n"
            + "ZjDtYrgW4rximI21bJhfveh2uCIhwGdPMu/5ZioVfvaOe8EOKqlI25sAazpK9gvY\\n"
            + "gOknHjHHaY2Coh7TeSHAnpJ7Y7Iz7/1myyIr+HWURRzinVmNf3JqlYCsgPTpXHxF\\n"
            + "SZlyi/9ugdxiwgMH9TI1KwcJt+fJxYOXPudjVybpJVrgjoYIzVExzF9OfjbnVrlz\\n"
            + "NtEzRx+kZEYNgMeAUk8ZpX6lis4nldo/gYX79t1utcaS3DutAkDjYl6mXCTRIIzW\\n"
            + "uKxCnZysnE9XxmB3Ul1JdBjn7BU+SVvVXumssAhzccNLhts10eUAJPloTLbmEwUC\\n"
            + "ZkMdkVhirVB5/01FaUxB88d9mdrF0tjM/LstCvvJFiP7AdHQKpFFVAdMgbwjzttg\\n"
            + "cGOkYQGuh5/ZxTMQnwf6EZNNEl5z764vUhmYTltybG+I6twyKpBEY7BYCW4ix8zr\\n"
            + "/qqe6SQMR8k6rDYK8k9EOnzEtw7ms96NHO3huCqsZOoLCxqMQPcSqcew45oGWcyR\\n"
            + "SJPuSg+AZdl9drYRAQiIZll9La74U6ERqsz5m6b3up9ZI0XqEIJnwvcWO8sZzjhK\\n"
            + "dNfeBt3Y9NG9725mlqksSwSaOGpWAI1DzFdqvtiDeQiUZ0i8fR02Y7pcRHCAxw==\\n"
            + "-----END CERTIFICATE-----";

    // Fixed placement.  three in site1, one is site 2
    static final String POST_BODY_DAML = "{"
                                         + "    \"consortium_id\": \"04e4f62d-5364-4363-a582-b397075b65a3\","
                                         + "    \"blockchain_type\": \"DAML\","
                                         + "    \"replica_zone_ids\": ["
                                         + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                         + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                         + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                         + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\"]"
                                         + "    ,\"replica_nodes\": ["
                                         + "{"
                                         + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                         + "             \"sizing_info\": {"
                                         + "             \"no_of_cpus\": \"2\","
                                         + "             \"storage_in_gigs\": \"60\","
                                         + "             \"memory_in_gigs\": \"32\"}" + "},"
                                         + "{"
                                         + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                         + "             \"sizing_info\": {"
                                         + "             \"no_of_cpus\": \"2\","
                                         + "             \"storage_in_gigs\": \"60\","
                                         + "             \"memory_in_gigs\": \"32\"}" + "},"
                                         + "{"
                                         + "            \"zone_id\": \"275638a3-8860-4925-85de-c73d45cb7232\","
                                         + "             \"sizing_info\": {"
                                         + "             \"no_of_cpus\": \"2\","
                                         + "             \"storage_in_gigs\": \"60\","
                                         + "             \"memory_in_gigs\": \"32\"}" + "},"
                                         + "{"
                                         + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                         + "             \"sizing_info\": {"
                                         + "             \"no_of_cpus\": \"2\","
                                         + "             \"storage_in_gigs\": \"60\","
                                         + "             \"memory_in_gigs\": \"32\"}" + "}"
                                         + "]"
                                         + "    ,\"client_nodes\": [{"
                                         + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                         + "            \"auth_url_jwt\": \"user@server.com\","
                                         + "            \"group_name\": \"1\","
                                         + "             \"sizing_info\": {"
                                         + "             \"no_of_cpus\": \"2\","
                                         + "             \"storage_in_gigs\": \"60\","
                                         + "             \"memory_in_gigs\": \"32\"},"
                                         + "            \"pem\": \"" + pem + "\","
                                         + "            \"crt\": \"" + crt + "\","
                                         + "            \"cacrt\": \"cacrt\""
                                         + "      }]"
                                         + "}";

    static final String POST_BODY_BAD_CONS = "{"
                                             + "    \"consortium_id\": \"a4b8f7ed-00b3-451e-97bc-4aa51a211288\","
                                             + "    \"blockchain_type\": \"DAML\","
                                             + "    \"replica_zone_ids\": ["
                                             + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                             + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                             + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                             + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\"]"
                                             + "    ,\"replica_nodes\": ["
                                             + "{"
                                             + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                             + "             \"sizing_info\": {"
                                             + "             \"no_of_cpus\": \"2\","
                                             + "             \"storage_in_gigs\": \"60\","
                                             + "             \"memory_in_gigs\": \"32\"}" + "},"
                                             + "{"
                                             + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                             + "             \"sizing_info\": {"
                                             + "             \"no_of_cpus\": \"2\","
                                             + "             \"storage_in_gigs\": \"60\","
                                             + "             \"memory_in_gigs\": \"32\"}" + "},"
                                             + "{"
                                             + "            \"zone_id\": \"275638a3-8860-4925-85de-c73d45cb7232\","
                                             + "             \"sizing_info\": {"
                                             + "             \"no_of_cpus\": \"2\","
                                             + "             \"storage_in_gigs\": \"60\","
                                             + "             \"memory_in_gigs\": \"32\"}" + "},"
                                             + "{"
                                             + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                             + "             \"sizing_info\": {"
                                             + "             \"no_of_cpus\": \"2\","
                                             + "             \"storage_in_gigs\": \"60\","
                                             + "             \"memory_in_gigs\": \"32\"}" + "}"
                                             + "]"
                                             + "}";

    static final String POST_BODY_ETH = "{"
                                        + "    \"consortium_id\": \"04e4f62d-5364-4363-a582-b397075b65a3\","
                                        + "    \"blockchain_type\": \"ETHEREUM\","
                                        + "    \"replica_zone_ids\": ["
                                        + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                        + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                        + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                        + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\"]"
                                        + "    ,\"replica_nodes\": ["
                                        + "{"
                                        + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                        + "             \"sizing_info\": {"
                                        + "             \"no_of_cpus\": \"2\","
                                        + "             \"storage_in_gigs\": \"60\","
                                        + "             \"memory_in_gigs\": \"32\"}" + "},"
                                        + "{"
                                        + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                        + "             \"sizing_info\": {"
                                        + "             \"no_of_cpus\": \"2\","
                                        + "             \"storage_in_gigs\": \"60\","
                                        + "             \"memory_in_gigs\": \"32\"}" + "},"
                                        + "{"
                                        + "            \"zone_id\": \"275638a3-8860-4925-85de-c73d45cb7232\","
                                        + "             \"sizing_info\": {"
                                        + "             \"no_of_cpus\": \"2\","
                                        + "             \"storage_in_gigs\": \"60\","
                                        + "             \"memory_in_gigs\": \"32\"}" + "},"
                                        + "{"
                                        + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                        + "             \"sizing_info\": {"
                                        + "             \"no_of_cpus\": \"2\","
                                        + "             \"storage_in_gigs\": \"60\","
                                        + "             \"memory_in_gigs\": \"32\"}" + "}"
                                        + "]"
                                        + "}";

    // Bad placement, wrong number of sites
    static final String POST_BODY_BAD = "{"
                                        + "    \"consortium_id\": \"04e4f62d-5364-4363-a582-b397075b65a3\","
                                        + "    \"f_count\": 1,"
                                        + "    \"c_count\": 0,"
                                        + "    \"replica_zone_ids\": ["
                                        + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                        + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                        + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                        + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\"]"
                                        + "    ,\"replica_nodes\": ["
                                        + "{"
                                        + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                        + "             \"sizing_info\": {"
                                        + "             \"no_of_cpus\": \"2\","
                                        + "             \"storage_in_gigs\": \"60\","
                                        + "             \"memory_in_gigs\": \"32\"}" + "},"
                                        + "{"
                                        + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                        + "             \"sizing_info\": {"
                                        + "             \"no_of_cpus\": \"2\","
                                        + "             \"storage_in_gigs\": \"60\","
                                        + "             \"memory_in_gigs\": \"32\"}" + "},"
                                        + "{"
                                        + "            \"zone_id\": \"275638a3-8860-4925-85de-c73d45cb7232\","
                                        + "             \"sizing_info\": {"
                                        + "             \"no_of_cpus\": \"2\","
                                        + "             \"storage_in_gigs\": \"60\","
                                        + "             \"memory_in_gigs\": \"32\"}" + "}" + "]" +  "}";

    static final String CORRECT_CLIENT_NUM = "{"
                                             + "    \"consortium_id\": \"04e4f62d-5364-4363-a582-b397075b65a3\","
                                             + "    \"blockchain_type\": \"DAML\","
                                             + "    \"replica_zone_ids\": ["
                                             + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                             + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                             + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                             + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\"]"
                                             + "    ,\"replica_nodes\": ["
                                             + "{"
                                             + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                             + "             \"sizing_info\": {"
                                             + "             \"no_of_cpus\": \"2\","
                                             + "             \"storage_in_gigs\": \"60\","
                                             + "             \"memory_in_gigs\": \"32\"}" + "},"
                                             + "{"
                                             + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                             + "             \"sizing_info\": {"
                                             + "             \"no_of_cpus\": \"2\","
                                             + "             \"storage_in_gigs\": \"60\","
                                             + "             \"memory_in_gigs\": \"32\"}" + "},"
                                             + "{"
                                             + "            \"zone_id\": \"275638a3-8860-4925-85de-c73d45cb7232\","
                                             + "             \"sizing_info\": {"
                                             + "             \"no_of_cpus\": \"2\","
                                             + "             \"storage_in_gigs\": \"60\","
                                             + "             \"memory_in_gigs\": \"32\"}" + "},"
                                             + "{"
                                             + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                             + "             \"sizing_info\": {"
                                             + "             \"no_of_cpus\": \"2\","
                                             + "             \"storage_in_gigs\": \"60\","
                                             + "             \"memory_in_gigs\": \"32\"}" + "}"
                                             + "]"
                                             + "    ,\"client_nodes\": [{"
                                             + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                             + "            \"auth_url_jwt\": \"user@server.com\","
                                             + "            \"group_name\": \"1\","
                                             + "             \"sizing_info\": {"
                                             + "             \"no_of_cpus\": \"2\","
                                             + "             \"storage_in_gigs\": \"60\","
                                             + "             \"memory_in_gigs\": \"32\"},"
                                             + "            \"pem\": \"" + pem + "\","
                                             + "            \"crt\": \"" + crt + "\","
                                             + "            \"cacrt\": \"cacrt\""
                                             + "      }]"
                                             + "}";

    static final String CLIENT_ETHEREUM_TYPE = "{"
                                               + "    \"consortium_id\": \"04e4f62d-5364-4363-a582-b397075b65a3\","
                                               + "    \"blockchain_type\": \"ETHEREUM\","
                                               + "    \"replica_zone_ids\": ["
                                               + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                               + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                               + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                               + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\"]"
                                               + "    ,\"replica_nodes\": ["
                                               + "{"
                                               + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                               + "             \"sizing_info\": {"
                                               + "             \"no_of_cpus\": \"2\","
                                               + "             \"storage_in_gigs\": \"60\","
                                               + "             \"memory_in_gigs\": \"32\"}" + "},"
                                               + "{"
                                               + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                               + "             \"sizing_info\": {"
                                               + "             \"no_of_cpus\": \"2\","
                                               + "             \"storage_in_gigs\": \"60\","
                                               + "             \"memory_in_gigs\": \"32\"}" + "},"
                                               + "{"
                                               + "            \"zone_id\": \"275638a3-8860-4925-85de-c73d45cb7232\","
                                               + "             \"sizing_info\": {"
                                               + "             \"no_of_cpus\": \"2\","
                                               + "             \"storage_in_gigs\": \"60\","
                                               + "             \"memory_in_gigs\": \"32\"}" + "},"
                                               + "{"
                                               + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                               + "             \"sizing_info\": {"
                                               + "             \"no_of_cpus\": \"2\","
                                               + "             \"storage_in_gigs\": \"60\","
                                               + "             \"memory_in_gigs\": \"32\"}" + "}"
                                               + "]"
                                               + "    ,\"client_nodes\": [{"
                                               + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                               + "            \"auth_url_jwt\": \"user@server.com\","
                                               + "            \"group_name\": \"1\","
                                               + "             \"sizing_info\": {"
                                               + "             \"no_of_cpus\": \"2\","
                                               + "             \"storage_in_gigs\": \"60\","
                                               + "             \"memory_in_gigs\": \"32\"},"
                                               + "            \"pem\": \"" + pem + "\","
                                               + "            \"crt\": \"" + crt + "\","
                                               + "            \"cacrt\": \"cacrt\""
                                               + "      }]"
                                               + "}";

    static final String WRONG_CLIENT_NUM = "{"
                                           + "    \"consortium_id\": \"04e4f62d-5364-4363-a582-b397075b65a3\","
                                           + "    \"blockchain_type\": \"DAML\","
                                           + "    \"replica_zone_ids\": ["
                                           + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                           + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\"]"
                                           + "    ,\"replica_nodes\": ["
                                           + "{"
                                           + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"}" + "},"
                                           + "{"
                                           + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"}" + "},"
                                           + "{"
                                           + "            \"zone_id\": \"275638a3-8860-4925-85de-c73d45cb7232\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"}" + "},"
                                           + "{"
                                           + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"}" + "}"
                                           + "]"
                                           + "    ,\"client_nodes\": ["
                                           + "{"
                                           + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"auth_url_jwt\": \"user@server.com\","
                                           + "            \"group_name\": \"Group 1\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"},"
                                           + "            \"pem\": \"" + pem + "\","
                                           + "            \"crt\": \"" + crt + "\","
                                           + "            \"cacrt\": \"cacrt\""
                                           + "},"
                                           + "{"
                                           + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"auth_url_jwt\": \"user@server.com\","
                                           + "            \"group_name\": \"Group 1\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"},"
                                           + "            \"pem\": \"" + pem + "\","
                                           + "            \"crt\": \"" + crt + "\","
                                           + "            \"cacrt\": \"cacrt\""
                                           + "},"
                                           + "{"
                                           + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"auth_url_jwt\": \"user@server.com\","
                                           + "            \"group_name\": \"Group 1\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"},"
                                           + "            \"pem\": \"" + pem + "\","
                                           + "            \"crt\": \"" + crt + "\","
                                           + "            \"cacrt\": \"cacrt\""
                                           + "},"
                                           + "{"
                                           + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"auth_url_jwt\": \"user@server.com\","
                                           + "            \"group_name\": \"Group 1\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"},"
                                           + "            \"pem\": \"" + pem + "\","
                                           + "            \"crt\": \"" + crt + "\","
                                           + "            \"cacrt\": \"cacrt\""
                                           + "},"
                                           + "{"
                                           + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"auth_url_jwt\": \"user@server.com\","
                                           + "            \"group_name\": \"Group 1\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"},"
                                           + "            \"pem\": \"" + pem + "\","
                                           + "            \"crt\": \"" + crt + "\","
                                           + "            \"cacrt\": \"cacrt\""
                                           + "},"
                                           + "{"
                                           + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"auth_url_jwt\": \"user@server.com\","
                                           + "            \"group_name\": \"Group 1\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"},"
                                           + "            \"pem\": \"" + pem + "\","
                                           + "            \"crt\": \"" + crt + "\","
                                           + "            \"cacrt\": \"cacrt\""
                                           + "},"
                                           + "{"
                                           + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"auth_url_jwt\": \"user@server.com\","
                                           + "            \"group_name\": \"Group 1\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"},"
                                           + "            \"pem\": \"" + pem + "\","
                                           + "            \"crt\": \"" + crt + "\","
                                           + "            \"cacrt\": \"cacrt\""
                                           + "},"
                                           + "{"
                                           + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"auth_url_jwt\": \"user@server.com\","
                                           + "            \"group_name\": \"Group 1\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"},"
                                           + "            \"pem\": \"" + pem + "\","
                                           + "            \"crt\": \"" + crt + "\","
                                           + "            \"cacrt\": \"cacrt\""
                                           + "},"
                                           + "{"
                                           + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"auth_url_jwt\": \"user@server.com\","
                                           + "            \"group_name\": \"Group 1\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"},"
                                           + "            \"pem\": \"" + pem + "\","
                                           + "            \"crt\": \"" + crt + "\","
                                           + "            \"cacrt\": \"cacrt\""
                                           + "},"
                                           + "{"
                                           + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"auth_url_jwt\": \"user@server.com\","
                                           + "            \"group_name\": \"Group 1\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"},"
                                           + "            \"pem\": \"" + pem + "\","
                                           + "            \"crt\": \"" + crt + "\","
                                           + "            \"cacrt\": \"cacrt\""
                                           + "},"
                                           + "{"
                                           + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"auth_url_jwt\": \"user@server.com\","
                                           + "            \"group_name\": \"Group 1\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"},"
                                           + "            \"pem\": \"" + pem + "\","
                                           + "            \"crt\": \"" + crt + "\","
                                           + "            \"cacrt\": \"cacrt\""
                                           + "}]"
                                           + "}";

    static final String BAD_NUM_REPLICAS = "{"
                                           + "    \"consortium_id\": \"04e4f62d-5364-4363-a582-b397075b65a3\","
                                           + "    \"blockchain_type\": \"DAML\","
                                           + "    \"replica_zone_ids\": ["
                                           + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\"]"
                                           + "    ,\"replica_nodes\": ["
                                           + "{"
                                           + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"}" + "},"
                                           + "{"
                                           + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"}" + "},"
                                           + "{"
                                           + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"}" + "}"
                                           + "]"
                                           + "    ,\"client_nodes\": [{"
                                           + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"auth_url_jwt\": \"user@server.com\","
                                           + "            \"group_name\": \"Group 1\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"},"
                                           + "            \"pem\": \"" + pem + "\","
                                           + "            \"crt\": \"" + crt + "\","
                                           + "            \"cacrt\": \"cacrt\""
                                           + "      }]"
                                           + "}";

    static final String CORRECT_NUM_REPLICAS = "{"
                                               + "    \"consortium_id\": \"04e4f62d-5364-4363-a582-b397075b65a3\","
                                               + "    \"blockchain_type\": \"DAML\","
                                               + "    \"replica_zone_ids\": ["
                                               + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                               + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                               + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                               + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                               + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                               + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                               + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\"]"
                                               + "    ,\"replica_nodes\": ["
                                               + "{"
                                               + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                               + "             \"sizing_info\": {"
                                               + "             \"no_of_cpus\": \"2\","
                                               + "             \"storage_in_gigs\": \"60\","
                                               + "             \"memory_in_gigs\": \"32\"}" + "},"
                                               + "{"
                                               + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                               + "             \"sizing_info\": {"
                                               + "             \"no_of_cpus\": \"2\","
                                               + "             \"storage_in_gigs\": \"60\","
                                               + "             \"memory_in_gigs\": \"32\"}" + "},"
                                               + "{"
                                               + "            \"zone_id\": \"275638a3-8860-4925-85de-c73d45cb7232\","
                                               + "             \"sizing_info\": {"
                                               + "             \"no_of_cpus\": \"2\","
                                               + "             \"storage_in_gigs\": \"60\","
                                               + "             \"memory_in_gigs\": \"32\"}" + "},"
                                               + "{"
                                               + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                               + "             \"sizing_info\": {"
                                               + "             \"no_of_cpus\": \"2\","
                                               + "             \"storage_in_gigs\": \"60\","
                                               + "             \"memory_in_gigs\": \"32\"}" + "}"
                                               + "]"
                                               + "    ,\"client_nodes\": [{"
                                               + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                               + "            \"auth_url_jwt\": \"user@server.com\","
                                               + "            \"group_name\": \"Group 1\","
                                               + "             \"sizing_info\": {"
                                               + "             \"no_of_cpus\": \"2\","
                                               + "             \"storage_in_gigs\": \"60\","
                                               + "             \"memory_in_gigs\": \"32\"},"
                                               + "            \"pem\": \"" + pem + "\","
                                               + "            \"crt\": \"" + crt + "\","
                                               + "            \"cacrt\": \"cacrt\""
                                               + "      }]"
                                               + "}";

    static final String BC_DAML_NO_CLIENT_GROUPING = "{"
                                                    + "    \"consortium_id\": \"04e4f62d-5364-4363-a582-b397075b65a3\","
                                                     + "    \"blockchain_type\": \"DAML\","
                                                     + "    \"replica_zone_ids\": ["
                                                     + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                                     + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                                     + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                                     + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\"]"
                                                     + "    ,\"replica_nodes\": ["
                                                     + "{"
                                                     + "     \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                                     + "             \"sizing_info\": {"
                                                     + "             \"no_of_cpus\": \"2\","
                                                     + "             \"storage_in_gigs\": \"60\","
                                                     + "             \"memory_in_gigs\": \"32\"}" + "},"
                                                     + "{"
                                                     + "       \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                                     + "             \"sizing_info\": {"
                                                     + "             \"no_of_cpus\": \"2\","
                                                     + "             \"storage_in_gigs\": \"60\","
                                                     + "             \"memory_in_gigs\": \"32\"}" + "},"
                                                     + "{"
                                                     + "       \"zone_id\": \"275638a3-8860-4925-85de-c73d45cb7232\","
                                                     + "             \"sizing_info\": {"
                                                     + "             \"no_of_cpus\": \"2\","
                                                     + "             \"storage_in_gigs\": \"60\","
                                                     + "             \"memory_in_gigs\": \"32\"}" + "},"
                                                     + "{"
                                                     + "        \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                                     + "             \"sizing_info\": {"
                                                     + "             \"no_of_cpus\": \"2\","
                                                     + "             \"storage_in_gigs\": \"60\","
                                                     + "             \"memory_in_gigs\": \"32\"}" + "}"
                                                     + "]"
                                                     + "    ,\"client_nodes\": [{"
                                                     + "      \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                                     + "      \"auth_url_jwt\": \"user@server.com\","
                                                     + "      \"sizing_info\": {"
                                                     + "        \"no_of_cpus\": \"2\","
                                                     + "        \"storage_in_gigs\": \"60\","
                                                     + "        \"memory_in_gigs\": \"32\"},"
                                                     + "      \"pem\": \"" + pem + "\","
                                                     + "      \"crt\": \"" + crt + "\","
                                                     + "      \"cacrt\": \"cacrt\""
                                                     + "      }]"
                                                     + "}";
    static final String NO_TLS_POST_BODY = "{"
                                           + "    \"consortium_id\": \"04e4f62d-5364-4363-a582-b397075b65a3\","
                                           + "    \"blockchain_type\": \"DAML\","
                                           + "    \"replica_zone_ids\": ["
                                           + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                           + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                           + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                           + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\"]"
                                           + "    ,\"replica_nodes\": ["
                                           + "{"
                                           + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"}" + "},"
                                           + "{"
                                           + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"}" + "},"
                                           + "{"
                                           + "            \"zone_id\": \"275638a3-8860-4925-85de-c73d45cb7232\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"}" + "},"
                                           + "{"
                                           + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"}" + "}"
                                           + "]"
                                           + "    ,\"client_nodes\": [{"
                                           + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                           + "            \"auth_url_jwt\": \"user@server.com\","
                                           + "            \"group_name\": \"Group 1\","
                                           + "             \"sizing_info\": {"
                                           + "             \"no_of_cpus\": \"2\","
                                           + "             \"storage_in_gigs\": \"60\","
                                           + "             \"memory_in_gigs\": \"32\"}"
                                           + "      }]"
                                           + "}";

    static final String MISSING_TLS_DETAILS = "{"
                                              + "    \"consortium_id\": \"04e4f62d-5364-4363-a582-b397075b65a3\","
                                              + "    \"blockchain_type\": \"DAML\","
                                              + "    \"replica_zone_ids\": ["
                                              + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                              + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                              + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                              + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                              + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                              + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                              + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\"]"
                                              + "    ,\"replica_nodes\": ["
                                              + "{"
                                              + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                              + "             \"sizing_info\": {"
                                              + "             \"no_of_cpus\": \"2\","
                                              + "             \"storage_in_gigs\": \"60\","
                                              + "             \"memory_in_gigs\": \"32\"}" + "},"
                                              + "{"
                                              + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                              + "             \"sizing_info\": {"
                                              + "             \"no_of_cpus\": \"2\","
                                              + "             \"storage_in_gigs\": \"60\","
                                              + "             \"memory_in_gigs\": \"32\"}" + "},"
                                              + "{"
                                              + "            \"zone_id\": \"275638a3-8860-4925-85de-c73d45cb7232\","
                                              + "             \"sizing_info\": {"
                                              + "             \"no_of_cpus\": \"2\","
                                              + "             \"storage_in_gigs\": \"60\","
                                              + "             \"memory_in_gigs\": \"32\"}" + "},"
                                              + "{"
                                              + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                              + "             \"sizing_info\": {"
                                              + "             \"no_of_cpus\": \"2\","
                                              + "             \"storage_in_gigs\": \"60\","
                                              + "             \"memory_in_gigs\": \"32\"}" + "}"
                                              + "]"
                                              + "    ,\"client_nodes\": [{"
                                              + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                              + "            \"auth_url_jwt\": \"user@server.com\","
                                              + "            \"group_name\": \"Group 1\","
                                              + "             \"sizing_info\": {"
                                              + "             \"no_of_cpus\": \"2\","
                                              + "             \"storage_in_gigs\": \"60\","
                                              + "             \"memory_in_gigs\": \"32\"},"
                                              + "            \"pem\": \"" + pem + "\","
                                              + "            \"cacrt\": \"cacrt\""
                                              + "      }]"
                                              + "}";

    static final String POST_BAD_PEM = "{"
                                            + "    \"consortium_id\": \"04e4f62d-5364-4363-a582-b397075b65a3\","
                                            + "    \"blockchain_type\": \"DAML\","
                                            + "    \"replica_zone_ids\": ["
                                            + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                            + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                            + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                            + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\"]"
                                            + "    ,\"replica_nodes\": ["
                                            + "{"
                                            + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                            + "             \"sizing_info\": {"
                                            + "             \"no_of_cpus\": \"2\","
                                            + "             \"storage_in_gigs\": \"60\","
                                            + "             \"memory_in_gigs\": \"32\"}" + "},"
                                            + "{"
                                            + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                            + "             \"sizing_info\": {"
                                            + "             \"no_of_cpus\": \"2\","
                                            + "             \"storage_in_gigs\": \"60\","
                                            + "             \"memory_in_gigs\": \"32\"}" + "},"
                                            + "{"
                                            + "            \"zone_id\": \"275638a3-8860-4925-85de-c73d45cb7232\","
                                            + "             \"sizing_info\": {"
                                            + "             \"no_of_cpus\": \"2\","
                                            + "             \"storage_in_gigs\": \"60\","
                                            + "             \"memory_in_gigs\": \"32\"}" + "},"
                                            + "{"
                                            + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                            + "             \"sizing_info\": {"
                                            + "             \"no_of_cpus\": \"2\","
                                            + "             \"storage_in_gigs\": \"60\","
                                            + "             \"memory_in_gigs\": \"32\"}" + "}"
                                            + "]"
                                            + "    ,\"client_nodes\": [{"
                                            + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                            + "            \"auth_url_jwt\": \"user@server.com\","
                                            + "            \"group_name\": \"1\","
                                            + "             \"sizing_info\": {"
                                            + "             \"no_of_cpus\": \"2\","
                                            + "             \"storage_in_gigs\": \"60\","
                                            + "             \"memory_in_gigs\": \"32\"},"
                                            + "            \"pem\": \"this_is_not_pem_this_is_katsura\","
                                            + "            \"crt\": \"" + crt + "\","
                                            + "            \"cacrt\": \"cacrt\""
                                            + "      }]"
                                            + "}";

    static final String POST_BAD_CRT = "{"
                                            + "    \"consortium_id\": \"04e4f62d-5364-4363-a582-b397075b65a3\","
                                            + "    \"blockchain_type\": \"DAML\","
                                            + "    \"replica_zone_ids\": ["
                                            + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                            + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                            + "            \"275638a3-8860-4925-85de-c73d45cb7232\","
                                            + "            \"84b9a0ed-c162-446a-b8c0-2e45755f3844\"]"
                                            + "    ,\"replica_nodes\": ["
                                            + "{"
                                            + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                            + "             \"sizing_info\": {"
                                            + "             \"no_of_cpus\": \"2\","
                                            + "             \"storage_in_gigs\": \"60\","
                                            + "             \"memory_in_gigs\": \"32\"}" + "},"
                                            + "{"
                                            + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                            + "             \"sizing_info\": {"
                                            + "             \"no_of_cpus\": \"2\","
                                            + "             \"storage_in_gigs\": \"60\","
                                            + "             \"memory_in_gigs\": \"32\"}" + "},"
                                            + "{"
                                            + "            \"zone_id\": \"275638a3-8860-4925-85de-c73d45cb7232\","
                                            + "             \"sizing_info\": {"
                                            + "             \"no_of_cpus\": \"2\","
                                            + "             \"storage_in_gigs\": \"60\","
                                            + "             \"memory_in_gigs\": \"32\"}" + "},"
                                            + "{"
                                            + "             \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                            + "             \"sizing_info\": {"
                                            + "             \"no_of_cpus\": \"2\","
                                            + "             \"storage_in_gigs\": \"60\","
                                            + "             \"memory_in_gigs\": \"32\"}" + "}"
                                            + "]"
                                            + "    ,\"client_nodes\": [{"
                                            + "            \"zone_id\": \"84b9a0ed-c162-446a-b8c0-2e45755f3844\","
                                            + "            \"auth_url_jwt\": \"user@server.com\","
                                            + "            \"group_name\": \"1\","
                                            + "             \"sizing_info\": {"
                                            + "             \"no_of_cpus\": \"2\","
                                            + "             \"storage_in_gigs\": \"60\","
                                            + "             \"memory_in_gigs\": \"32\"},"
                                            + "            \"pem\": \"" + pem + "\","
                                            + "            \"crt\": \"this_is_not_crt_this_is_abbe_bussoni\","
                                            + "            \"cacrt\": \"cacrt\""
                                            + "      }]"
                                            + "}";

    private static final UUID CLIENT_NODE_ID = UUID.fromString("7eef6110-68bc-11ea-906e-8c859085f3e7");
    private static final UUID CLIENT_GROUP_ID = UUID.fromString("050d3785-e2fc-4b59-9042-191da02a81a9");
    private static final String CLIENT_GROUP_NAME = "Test Group";
    private static final UUID DEFAULT_TEMPLATE_ID = UUID.fromString("dab730c9-c82d-436a-8b71-38076f061093");
    private static final String DEFAULT_TEMPLATE_NAME = "Default Template";


    @Autowired
    private WebApplicationContext context;

    private MockMvc mockMvc;

    @MockBean
    DefaultProfiles defaultProfiles;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    @MockBean
    ConsortiumService consortiumService;

    @MockBean
    BlockchainService blockchainService;

    @MockBean
    ProvisioningServiceV2Grpc.ProvisioningServiceV2Stub client;

    @MockBean
    OperationContext operationContext;

    @MockBean
    ConcordService concordService;

    @MockBean
    ZoneService zoneService;

    @MockBean
    ClientService clientService;

    @MockBean
    OrganizationService organizationService;

    @MockBean
    NodeSizeTemplateService nodeSizeTemplateService;

    @Autowired
    TaskService taskService;

    @Autowired
    AuthHelper authHelper;

    private Consortium consortium;
    private ObjectMapper objectMapper;

    private AuthenticationContext adminAuth;
    private AuthenticationContext consortiumAuth;
    private AuthenticationContext userAuth;
    private AuthenticationContext user2Auth;

    private void setCreateCluster(Answer answer) {
        doAnswer(answer).when(client)
                .createDeployment(any(DeploymentRequest.class), any(StreamObserver.class));
    }

    private void setStreamCluster(Answer answer) {
        doAnswer(answer).when(client)
                .streamDeploymentSessionEvents(any(StreamDeploymentSessionEventRequest.class),
                                               any(StreamObserver.class));
    }

    /**
     * A mockito Answer that saves the result of the mock, so we can
     * look at it later.
     */
    static class ResultAnswer<T> implements Answer {
        T result;
        Function<InvocationOnMock, T> function;

        public ResultAnswer(Function<InvocationOnMock, T> function) {
            this.function = function;
        }

        public T getResult() {
            return result;
        }

        @Override
        public Object answer(InvocationOnMock invocation) throws Throwable {
            result = function.apply(invocation);
            return result;
        }
    }

    private ResultAnswer<Blockchain> blockchainResultAnswer;

    /**
     * Initialize various mocks.
     */
    @BeforeEach
    public void init() {
        mockMvc = MockMvcBuilders
                .webAppContextSetup(context)
                .apply(springSecurity())
                .build();
        consortium = SecurityTestUtils.getConsortium();
        when(operationContext.getId()).thenReturn(UUID.randomUUID().toString());
        Consortium c2 = new Consortium();
        c2.setId(C2_ID);
        Consortium c3 = new Consortium();
        c3.setId(C3_ID);
        UUID c1Id = consortium.getId();
        when(consortiumService.get(c1Id)).thenReturn(consortium);
        when(consortiumService.get(C2_ID)).thenReturn(c2);
        when(consortiumService.get(C3_ID)).thenReturn(c3);

        final Blockchain b = Blockchain.builder()
                .consortium(consortium.getId())
                .nodeList(Stream.of("1", "2", "3")
                                  .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, s, "", "", SITE_1))
                                  .collect(Collectors.toList()))
                .build();

        final Blockchain b2 = Blockchain.builder()
                .consortium(c2.getId())
                .nodeList(Stream.of("4", "5", "6")
                                  .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, s, "", "", SITE_2))
                                  .collect(Collectors.toList()))
                .build();

        final Blockchain bcInactive = Blockchain.builder()
                .consortium(consortium.getId())
                .nodeList(Stream.of("one", "two", "three")
                                  .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, s, "http://".concat(s), "cert-".concat(s), SITE_2))
                                  .collect(Collectors.toList()))
                .state(Blockchain.BlockchainState.INACTIVE)
                .type(BlockchainType.ETHEREUM)
                .build();

        final Blockchain bn = Blockchain.builder()
                .consortium(UUID.fromString("04e4f62d-5364-4363-a582-b397075b65a3"))
                .nodeList(Stream.of("one", "two", "three")
                                  .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, s, "http://".concat(s), "cert-".concat(s), SITE_2))
                                  .collect(Collectors.toList()))
                .build();

        final Blockchain bcdaml = Blockchain.builder()
                .consortium(UUID.fromString("5a0cebc0-057e-11ea-8d71-362b9e155667"))
                .nodeList(Stream.of("one", "two", "three")
                                  .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, s, "http://".concat(s), "cert-".concat(s), SITE_2))
                                  .collect(Collectors.toList()))
                .type(BlockchainType.DAML)
                .build();

        final Blockchain bcDeregister = Blockchain.builder()
                .consortium(UUID.fromString("5a0cebc0-057e-11ea-8d71-362b9e155667"))
                .nodeList(Stream.of("one", "two", "three")
                                  .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, s, "http://".concat(s), "cert-".concat(s), SITE_2))
                                  .collect(Collectors.toList()))
                .type(BlockchainType.ETHEREUM)
                .build();

        final Blockchain bcDeregisterInactive = Blockchain.builder()
                .consortium(UUID.fromString("5a0cebc0-057e-11ea-8d71-362b9e155667"))
                .nodeList(Stream.of("one", "two", "three")
                                  .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, s, "http://".concat(s), "cert-".concat(s), SITE_2))
                                  .collect(Collectors.toList()))
                .state(Blockchain.BlockchainState.INACTIVE)
                .type(BlockchainType.ETHEREUM)
                .build();

        when(blockchainService.listByConsortium(consortium)).thenReturn(Collections.singletonList(b));
        when(blockchainService.listByConsortium(c3)).thenReturn(Collections.emptyList());
        when(blockchainService.list()).thenReturn(ImmutableList.of(b, b2, bcInactive));
        when(blockchainService.put(any(Blockchain.class)))
                .thenAnswer(invocation -> {
                    Blockchain blockchain = invocation.getArgument(0);
                    return blockchain;
                });
        b.setId(BC_ID);
        b2.setId(BC2_ID);
        bn.setId(BC_NEW);
        bcdaml.setId(BC_DAML);
        bcDeregister.setId(BC_DEREGISTER);
        bcDeregisterInactive.setId(BC_DEREGISTER_INACTIVE);
        when(blockchainService.get(BC_ID)).thenReturn(b);
        when(blockchainService.get(BC2_ID)).thenReturn(b2);
        when(blockchainService.get(BC_NEW)).thenReturn(bn);
        when(blockchainService.get(BC_DAML)).thenReturn(bcdaml);
        when(blockchainService.get(BC_DEREGISTER)).thenReturn(bcDeregister);
        when(blockchainService.get(BC_DEREGISTER_INACTIVE)).thenReturn(bcDeregisterInactive);
        when(blockchainService.get(BC_MISSING)).thenReturn(null);
        when(blockchainService.get(C2_ID)).thenThrow(new NotFoundException("Not found"));
        when(blockchainService.listByIds(any(List.class))).thenAnswer(i -> {
            return ((List<UUID>) i.getArgument(0)).stream().map(blockchainService::get).collect(Collectors.toList());
        });
        blockchainResultAnswer = new ResultAnswer<>(i -> {
            Blockchain bc = new Blockchain.BlockchainBuilder()
                    .consortium(i.getArgument(1))
                    .type(i.getArgument(2))
                    .build();
            bc.setId(i.getArgument(0));
            return bc;
        });
        when(blockchainService.create(any(), any(), any(), any()))
                .thenAnswer(blockchainResultAnswer);
        when(defaultProfiles.getBlockchain()).thenReturn(bn);
        Task t = new Task();
        t.setId(TASK_ID);
        t.setState(State.SUCCEEDED);
        t.setMessage("Done");
        t.setResourceId(BC_NEW);
        when(taskService.put(any())).thenReturn(t);
        when(taskService.get(TASK_ID)).thenReturn(t);
        // This creates our default object mapper
        objectMapper = jacksonBuilder.build();

        // Create authorizations for the different users.
        adminAuth = createContext("operator", ORG_ID,
                                  ImmutableList.of(VmbcRoles.SYSTEM_ADMIN, VmbcRoles.ORG_USER),
                                  ImmutableList.of(C2_ID),
                                  ImmutableList.of(BC_ID), "");

        consortiumAuth = createContext("consortium", ORG_ID,
                                       ImmutableList.of(VmbcRoles.CONSORTIUM_ADMIN, VmbcRoles.ORG_USER),
                                       ImmutableList.of(C2_ID),
                                       Collections.emptyList(), "");

        userAuth = createContext("operator", ORG_ID,
                                 ImmutableList.of(VmbcRoles.ORG_USER),
                                 ImmutableList.of(C2_ID),
                                 ImmutableList.of(BC_ID), "");

        user2Auth = createContext("operator", ORG2_ID,
                                  ImmutableList.of(VmbcRoles.ORG_USER),
                                  ImmutableList.of(C3_ID),
                                  Collections.emptyList(), "");

        setCreateCluster(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(DeploymentRequestResponse.newBuilder().setId(DEP_ID.toString()).build());
            ob.onCompleted();
            return null;
        });

        // Zone returns
        when(zoneService.get(SITE_1)).thenReturn(ZoneTestUtils.getOnpremZone(SITE_1, ORG_ID));

        when(organizationService.get(any(UUID.class))).thenReturn(mock(Organization.class));

        VmcAwsZone vmcAwsZone = new VmcAwsZone();
        vmcAwsZone.setType(Zone.Type.VMC_AWS);
        vmcAwsZone.setId(SITE_2);
        vmcAwsZone.setNetwork(new Zone.Network("name", null, "10.10.10.10", "24",
                                               new ArrayList<>()));
        vmcAwsZone.setResourcePool("resource");
        vmcAwsZone.setFolder("folder");
        vmcAwsZone.setDatacenter("dc");
        vmcAwsZone.setStorage("storage");
        vmcAwsZone.setVmcUrl("vmc");
        vmcAwsZone.setCspUrl("csp");
        vmcAwsZone.setRefreshToken("rt");
        vmcAwsZone.setOrganization("org");
        when(zoneService.get(SITE_2)).thenReturn(vmcAwsZone);

        NodeSizeTemplate nst = NodeSizeTemplateUtil.createNodeSizeTemplate(DEFAULT_TEMPLATE_ID, DEFAULT_TEMPLATE_NAME);

        when(nodeSizeTemplateService.getTemplate()).thenReturn(nst);
    }

    @Test
    public void testUpdateBlockchainVersion() throws Exception {
        String patchValues = "{\n"
                             + "    \"blockchain_version\": \"new version\"\n"
                             + "}";

        Blockchain bNewVersion = new Blockchain();
        bNewVersion.setId(UUID.randomUUID());

        when(blockchainService.update(any(Blockchain.class), any(BlockchainPatch.class))).thenReturn(bNewVersion);
        mockMvc.perform(patch("/api/blockchains/" + BC_ID.toString()).with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(patchValues)
                                .characterEncoding("utf-8"))
                .andExpect(status().isAccepted());
    }

    @Test
    public void testUpdateBlockchainVersionAndExecEngineVersion() throws Exception {
        String patchValues = "{\n"
                             + "    \"blockchain_version\": \"new version\",\n"
                             + "    \"execution_engine_version\": \"new exec eng version\"\n"
                             + "}";

        Blockchain bNewVersion = new Blockchain();
        bNewVersion.setId(UUID.randomUUID());

        when(blockchainService.update(any(Blockchain.class), any(BlockchainPatch.class))).thenReturn(bNewVersion);
        mockMvc.perform(patch("/api/blockchains/" + BC_ID.toString()).with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(patchValues)
                                .characterEncoding("utf-8"))
                .andExpect(status().isAccepted());
    }

    @Test
    public void testUpdateExecEngineVersion() throws Exception {
        String patchValues = "{\n"
                             + "    \"execution_engine_version\": \"new exec eng version\"\n"
                             + "}";

        Blockchain bNewVersion = new Blockchain();
        bNewVersion.setId(UUID.randomUUID());

        when(blockchainService.update(any(Blockchain.class), any(BlockchainPatch.class))).thenReturn(bNewVersion);
        mockMvc.perform(patch("/api/blockchains/" + BC_ID.toString()).with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(patchValues)
                                .characterEncoding("utf-8"))
                .andExpect(status().isAccepted());
    }

    @Test
    public void testUpdateBlockchainVersionUnsuccessful() throws Exception {
        String patchValues = "{\n"
                              + "    \"blockchain_version\": \"new version\"\n"
                              + "}";

        when(blockchainService.update(any(Blockchain.class), any(BlockchainPatch.class))).thenThrow(
                new RuntimeException("test"));
        mockMvc.perform(patch("/api/blockchains/" + BC_ID.toString()).with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(patchValues)
                                .characterEncoding("utf-8"))
                .andExpect(status().isInternalServerError());
    }

    @Test
    void getBlockchainOperatorListActiveOnly() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/blockchains/").with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<BlockchainApiObjects.BlockchainGetResponse> res =
                objectMapper.readValue(body, new TypeReference<List<BlockchainApiObjects.BlockchainGetResponse>>() {});
        // As an operator, we should see both blockchains.
        Assertions.assertEquals(2, res.size());
    }

    @Test
    void getBlockchainOperatorListAllBlockchains() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/blockchains?all=true").with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<BlockchainApiObjects.BlockchainGetResponse> res =
                objectMapper.readValue(body, new TypeReference<List<BlockchainApiObjects.BlockchainGetResponse>>() {});
        // As an operator, we should see both blockchains.
        Assertions.assertEquals(3, res.size());

        // Check if the third blockchain is inactive
        Assertions.assertEquals(Blockchain.BlockchainState.ACTIVE, res.get(0).getBlockchainState());
        Assertions.assertEquals(Blockchain.BlockchainState.ACTIVE, res.get(1).getBlockchainState());
        Assertions.assertEquals(Blockchain.BlockchainState.INACTIVE, res.get(2).getBlockchainState());
    }

    @Test
    void getBlockchainUserList() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/blockchains/").with(authentication(userAuth))
                                                   .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<BlockchainApiObjects.BlockchainGetResponse> res = objectMapper.readValue(body, new TypeReference<>() {});
        // As a user in this consortium, we should only see one blockchain
        Assertions.assertEquals(1, res.size());
    }

    @Test
    void getBlockchainUser2List() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/blockchains/").with(authentication(user2Auth))
                                                   .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<BlockchainApiObjects.BlockchainGetResponse> res = objectMapper.readValue(body, new TypeReference<>() {});
        // As a user in this consortium, we should only see one blockchain
        Assertions.assertEquals(0, res.size());
    }

    @Test
    void getBlockchainOperator() throws Exception {
        mockMvc.perform(get("/api/blockchains/" + BC_ID.toString()).with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void getBlockchainOperatorNotFound() throws Exception {
        // There is no such blockchain.
        MvcResult result = mockMvc.perform(get("/api/blockchains/" + C2_ID.toString()).with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound()).andReturn();

        String body = result.getResponse().getContentAsString();

        String message = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(MessageFormat.format(ErrorCode.BLOCKCHAIN_NOT_FOUND, C2_ID.toString()), message);
    }

    @Test
    void getMissingBlockchain() throws Exception {
        // There is no such blockchain.
        MvcResult result = mockMvc.perform(
                get("/api/blockchains/" + BC_MISSING.toString()).with(authentication(adminAuth))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound()).andReturn();

        String body = result.getResponse().getContentAsString();

        String message = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(MessageFormat.format(ErrorCode.BLOCKCHAIN_NOT_FOUND, BC_MISSING.toString()), message);
    }

    @Test
    void getBlockchainUser() throws Exception {
        mockMvc.perform(get("/api/blockchains/" + BC_ID.toString()).with(authentication(userAuth))
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void getBlockchainOperatorBc2() throws Exception {
        mockMvc.perform(get("/api/blockchains/" + BC2_ID.toString()).with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    void getBlockchainUserBc2() throws Exception {
        mockMvc.perform(get("/api/blockchains/" + BC2_ID.toString()).with(authentication(userAuth))
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isForbidden());
    }

    @Test
    void getBlockchainNoAccess() throws Exception {
        mockMvc.perform(get("/api/blockchains/" + BC_ID.toString())
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isUnauthorized());
    }

    @Test
    void getBlockchainUnavailable() throws Exception {
        MvcResult result =
                mockMvc.perform(
                        get("/api/blockchains/" + BC_UNAVAILABLE.toString()).with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON))
                        .andExpect(status().isNotFound()).andReturn();

        String body = result.getResponse().getContentAsString();

        String message = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(MessageFormat.format(ErrorCode.BLOCKCHAIN_NOT_FOUND, BC_UNAVAILABLE), message);
    }

    @Test
    void postUserAccess() throws Exception {
        mockMvc.perform(post("/api/blockchains").with(authentication(userAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_BODY_DAML)
                                .characterEncoding("utf-8"))
                .andExpect(status().isForbidden());
    }

    @Test
    void postWrongConsortium() throws Exception {
        mockMvc.perform(post("/api/blockchains").with(authentication(consortiumAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_BODY_BAD_CONS)
                                .characterEncoding("utf-8"))
                .andExpect(status().isForbidden());
    }

    @Test
    void createTooMany() throws Exception {
        AuthenticationContext tooManyAuth = createContext("operator", ORG_ID,
                                                      ImmutableList.of(VmbcRoles.CONSORTIUM_ADMIN, VmbcRoles.ORG_USER),
                                                      ImmutableList.of(C2_ID),
                                                      ImmutableList.of(BC_ID), "");
        mockMvc.perform(post("/api/blockchains").with(authentication(tooManyAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_BODY_DAML).characterEncoding("utf-8"))
                .andExpect(status().isBadRequest());
    }

    @Test
    void orgHasLimit() throws Exception {
        Organization org = new Organization();
        org.setId(ORG_ID);
        org.setOrganizationProperties(ImmutableMap.of(Constants.ORG_MAX_CHAINS, "2"));
        when(organizationService.get(ORG_ID)).thenReturn(org);
        AuthenticationContext tooManyAuth = createContext("operator", ORG_ID,
                                                          ImmutableList
                                                                  .of(VmbcRoles.CONSORTIUM_ADMIN, VmbcRoles.ORG_USER),
                                                          ImmutableList.of(C2_ID),
                                                          ImmutableList.of(BC_ID), "");
        mockMvc.perform(post("/api/blockchains").with(authentication(tooManyAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_BODY_DAML).characterEncoding("utf-8"))
                .andExpect(status().isAccepted());
    }

    @Test
    void orgHasLimitExceded() throws Exception {
        Organization org = new Organization();
        org.setId(ORG_ID);
        org.setOrganizationProperties(ImmutableMap.of(Constants.ORG_MAX_CHAINS, "2"));
        when(organizationService.get(ORG_ID)).thenReturn(org);
        AuthenticationContext tooManyAuth = createContext("operator", ORG_ID,
                                                          ImmutableList
                                                                  .of(VmbcRoles.CONSORTIUM_ADMIN, VmbcRoles.ORG_USER),
                                                          ImmutableList.of(C2_ID),
                                                          ImmutableList.of(BC_ID, BC2_ID), "");
        mockMvc.perform(post("/api/blockchains").with(authentication(tooManyAuth)));
        mockMvc.perform(post("/api/blockchains").with(authentication(tooManyAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_BODY_DAML).characterEncoding("utf-8"))
                .andExpect(status().isBadRequest());
    }

    @Test
    void orgHasUnlimited() throws Exception {
        Organization org = new Organization();
        org.setId(ORG_ID);
        org.setOrganizationProperties(ImmutableMap.of(Constants.ORG_MAX_CHAINS, "0"));
        when(organizationService.get(ORG_ID)).thenReturn(org);
        AuthenticationContext tooManyAuth = createContext("operator", ORG_ID,
                                                          ImmutableList
                                                                  .of(VmbcRoles.CONSORTIUM_ADMIN, VmbcRoles.ORG_USER),
                                                          ImmutableList.of(C2_ID),
                                                          ImmutableList.of(BC_ID, BC2_ID), "");
        mockMvc.perform(post("/api/blockchains").with(authentication(tooManyAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_BODY_DAML).characterEncoding("utf-8"))
                .andExpect(status().isAccepted());
    }

    @Test
    void createFixed() throws Exception {
        ArgumentCaptor<DeploymentRequest> captor = ArgumentCaptor.forClass(DeploymentRequest.class);
        mockMvc.perform(post("/api/blockchains").with(authentication(consortiumAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_BODY_DAML).characterEncoding("utf-8"))
                .andExpect(status().isAccepted());
        verify(client).createDeployment(captor.capture(), any(StreamObserver.class));
        DeploymentRequest request = captor.getValue();
    }

    @Test
    void creatBad() throws Exception {
        mockMvc.perform(post("/api/blockchains").with(authentication(consortiumAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_BODY_BAD).characterEncoding("utf-8"))
                .andExpect(status().isBadRequest());

    }

    @Test
    void postCreateCluster() throws Exception {

        setStreamCluster(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(buildEvent(DeployedResource.newBuilder().setNodeId(NODE_1.toString()).build(),
                                 DeploymentExecutionEvent.Type.ACKNOWLEDGED, Status.ACTIVE));
            ob.onNext(buildEvent(DeployedResource.newBuilder().setNodeId(NODE_1.toString()).build(),
                                 DeploymentExecutionEvent.Type.COMPLETED, Status.SUCCESS));
            ob.onCompleted();
            return null;
        });

        final Client client1 = new Client("publicIp", "privateIp", "hostName", "url",
                                          "cert", "pass", BC_DAML, SITE_1, CLIENT_GROUP_ID, CLIENT_GROUP_NAME,
                                          "pem", "crt", "cacrt");
        client1.setId(CLIENT_NODE_ID);
        when(clientService.getClientsByParentId(BC_DAML)).thenReturn(ImmutableList.of(client1));


        MvcResult result = mockMvc.perform(post("/api/blockchains").with(authentication(consortiumAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_BODY_DAML).characterEncoding("utf-8"))
                .andExpect(status().isAccepted()).andReturn();
        String body = result.getResponse().getContentAsString();

        BlockchainTaskResponse t = objectMapper.readValue(body, BlockchainTaskResponse.class);
        Assertions.assertEquals(TASK_ID, t.getTaskId());

        result = mockMvc.perform(get("/api/tasks/" + TASK_ID.toString())
                                         .with(authentication(consortiumAuth))
                                         .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        body = result.getResponse().getContentAsString();
        Map<String, String> r = objectMapper.readValue(body, new TypeReference<Map<String, String>>() {});
        Assertions.assertEquals(TASK_ID.toString(), r.get("task_id"));
        Assertions.assertEquals("SUCCEEDED", r.get("state"));
        Assertions.assertEquals("Operation finished", r.get("message"));
        Assertions.assertEquals(BC_NEW.toString(), r.get("resource_id"));
        Blockchain blockchain = blockchainResultAnswer.getResult();
        Assertions.assertEquals(BC_NEW, blockchain.getId());
        Assertions.assertEquals(BlockchainType.DAML, blockchain.getType());

        // Also verify if the client response is correct.
        //verifyClientApi();
    }

    @Test
    void deregisterBlockchain() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/deregister/" + BC_DEREGISTER.toString())
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isAccepted()).andReturn();

        String body = result.getResponse().getContentAsString();
        BlockchainApiObjects.BlockchainGetResponse res = objectMapper
                .readValue(body, BlockchainApiObjects.BlockchainGetResponse.class);
        // As a user in this consortium, we should only see one blockchain
        Assertions.assertEquals(Blockchain.BlockchainState.INACTIVE, res.getBlockchainState());
    }

    @Test
    void deregisterUnavailableBlockchain() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/deregister/" + BC_UNAVAILABLE.toString())
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound()).andReturn();

        String body = result.getResponse().getContentAsString();

        String message = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(MessageFormat.format(ErrorCode.BLOCKCHAIN_NOT_FOUND, BC_UNAVAILABLE), message);
    }

    @Test
    void deregisterMissingBlockchain() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/deregister/" + BC_MISSING.toString())
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound()).andReturn();

        String body = result.getResponse().getContentAsString();

        String message = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(MessageFormat.format(ErrorCode.BLOCKCHAIN_NOT_FOUND, BC_MISSING), message);
    }

    @Test
    void deregisterInactiveBlockchain() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/deregister/" + BC_DEREGISTER_INACTIVE.toString())
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest()).andReturn();

        Assertions.assertEquals(String.format("Blockchain %s is already de-registered.", BC_DEREGISTER_INACTIVE
                                        .toString()),
                                result.getResolvedException().getMessage());
    }

    DeploymentExecutionEvent buildEvent(DeployedResource resource, DeploymentExecutionEvent.Type type,
                                        DeploymentExecutionEvent.Status status) {
        return DeploymentExecutionEvent.newBuilder()
                .setResource(resource)
                .setStatus(status)
                .setType(type)
                .setSessionId(DEP_ID.toString())
                .setBlockchainId(BC_NEW.toString())
                .build();
    }

    @Test
    void correctClientNumberCheck() throws Exception {
        ArgumentCaptor<DeploymentRequest> captor = ArgumentCaptor.forClass(DeploymentRequest.class);
        mockMvc.perform(post("/api/blockchains").with(authentication(consortiumAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(CORRECT_CLIENT_NUM).characterEncoding("utf-8"))
                .andExpect(status().isAccepted());
        verify(client).createDeployment(captor.capture(), any(StreamObserver.class));
    }

    @Test
    void clientNumberEthereumCheck() throws Exception {
        ArgumentCaptor<DeploymentRequest> captor = ArgumentCaptor.forClass(DeploymentRequest.class);
        mockMvc.perform(post("/api/blockchains").with(authentication(consortiumAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(CLIENT_ETHEREUM_TYPE).characterEncoding("utf-8"))
                .andExpect(status().isAccepted());
        verify(client).createDeployment(captor.capture(), any(StreamObserver.class));
    }

    @Test
    void wrongClientNumberCheck() throws Exception {
        mockMvc.perform(post("/api/blockchains").with(authentication(consortiumAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(WRONG_CLIENT_NUM).characterEncoding("utf-8"))
                .andExpect(status().isBadRequest());
    }

    @Test
    void wrongReplicaNumber() throws Exception {
        mockMvc.perform(post("/api/blockchains").with(authentication(consortiumAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(BAD_NUM_REPLICAS).characterEncoding("utf-8"))
                .andExpect(status().isBadRequest());
    }

    @Test
    void correctReplicaNumber() throws Exception {
        mockMvc.perform(post("/api/blockchains").with(authentication(consortiumAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(CORRECT_NUM_REPLICAS).characterEncoding("utf-8"))
                .andExpect(status().isAccepted());
    }

    @Test
    void testBcWithNoGrouping() throws Exception {
        setStreamCluster(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(buildEvent(DeployedResource.newBuilder().setNodeId(NODE_1.toString()).build(),
                                 DeploymentExecutionEvent.Type.ACKNOWLEDGED, Status.ACTIVE));
            ob.onNext(buildEvent(DeployedResource.newBuilder().setNodeId(NODE_1.toString()).build(),
                                 DeploymentExecutionEvent.Type.COMPLETED, Status.SUCCESS));
            ob.onCompleted();
            return null;
        });

        // If client grouping feature is not in use, we use client node Id as client group Id.
        final Client client1 = new Client("publicIp", "privateIp", "hostName", "url",
                                          "cert",  "pass", BC_DAML, SITE_1, CLIENT_NODE_ID, null,
                                          "pem", "crt", "cacrt");
        client1.setId(CLIENT_NODE_ID);
        when(clientService.getClientsByParentId(BC_DAML)).thenReturn(ImmutableList.of(client1));

        MvcResult result = mockMvc.perform(post("/api/blockchains").with(authentication(consortiumAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(BC_DAML_NO_CLIENT_GROUPING).characterEncoding("utf-8"))
                .andExpect(status().isAccepted()).andReturn();
        String body = result.getResponse().getContentAsString();

        BlockchainTaskResponse t = objectMapper.readValue(body, BlockchainTaskResponse.class);
        Assertions.assertEquals(TASK_ID, t.getTaskId());

        result = mockMvc.perform(get("/api/tasks/" + TASK_ID.toString())
                                         .with(authentication(consortiumAuth))
                                         .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        body = result.getResponse().getContentAsString();
        Map<String, String> r = objectMapper.readValue(body, new TypeReference<Map<String, String>>() {});
        Assertions.assertEquals(TASK_ID.toString(), r.get("task_id"));
        Assertions.assertEquals("SUCCEEDED", r.get("state"));
        Assertions.assertEquals("Operation finished", r.get("message"));
        Assertions.assertEquals(BC_NEW.toString(), r.get("resource_id"));
        Blockchain blockchain = blockchainResultAnswer.getResult();
        Assertions.assertEquals(BC_NEW, blockchain.getId());
        Assertions.assertEquals(BlockchainType.DAML, blockchain.getType());
    }

    @Test
    void noTlsBlockchainPost() throws Exception {
        mockMvc.perform(post("/api/blockchains").with(authentication(consortiumAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(NO_TLS_POST_BODY).characterEncoding("utf-8"))
                .andExpect(status().isAccepted());
    }

    @Test
    void missingTlsCredentialsPost() throws Exception {
        mockMvc.perform(post("/api/blockchains").with(authentication(consortiumAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(MISSING_TLS_DETAILS).characterEncoding("utf-8"))
                .andExpect(status().isBadRequest());
    }

    @Test
    void badPemPost() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains").with(authentication(consortiumAuth))
                .contentType(MediaType.APPLICATION_JSON)
                .content(POST_BAD_PEM).characterEncoding("utf-8"))
                .andExpect(status().isBadRequest()).andReturn();

        String body = result.getResponse().getContentAsString();

        String errorMessage = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(ErrorCode.BAD_TLS_CREDENTIALS_PEM, errorMessage);
    }

    @Test
    void badCrtPost() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains").with(authentication(consortiumAuth))
                .contentType(MediaType.APPLICATION_JSON)
                .content(POST_BAD_CRT).characterEncoding("utf-8"))
                .andExpect(status().isBadRequest()).andReturn();

        String body = result.getResponse().getContentAsString();

        String errorMessage = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(ErrorCode.BAD_TLS_CREDENTIALS_CRT, errorMessage);
    }

}
