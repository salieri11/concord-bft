/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.connections.ConnectionPoolManager;
import com.vmware.blockchain.security.ServiceContext;
import com.vmware.blockchain.services.blockchains.Blockchain;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.blockchains.replicas.Replica;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.concord.ConcordService;
import com.vmware.concord.Concord.Peer;

/**
 * Tests for the DefaultProfiles component.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:default-test.properties")
@ComponentScan(basePackageClasses = {DefaultProfilesTest.class})
class DefaultProfilesTest {
    // Just some random UUIDs
    private static final UUID USER_ID = UUID.fromString("f1c1aa4f-4958-4e93-8a51-930d595fb65b");
    private static final UUID ORG_ID = UUID.fromString("82634974-88cf-4944-a99d-6b92664bb765");
    private static final UUID CONSORTIUM_ID = UUID.fromString("5c7cd0e9-57ad-44af-902f-74af2f3dd8fe");

    @MockBean
    ConsortiumService consortiumService;

    @MockBean
    OrganizationService organizationService;

    @MockBean
    UserService userService;

    @MockBean
    PasswordEncoder passwordEncoder;

    @MockBean
    BlockchainService blockchainService;

    @MockBean
    AgreementService agreementService;

    @MockBean
    ServiceContext serviceContext;

    @MockBean
    ReplicaService replicaService;

    @MockBean
    ConcordService concordService;

    @MockBean
    ConnectionPoolManager connectionPoolManager;

    DefaultProfiles profiles;

    @Value("${ConcordAuthorities}")
    private String ipList;
    @Value("${ConcordRpcUrls}")
    private String rpcUrls;
    @Value("${ConcordRpcCerts}")
    private String rpcCerts;
    private Consortium consortium;
    private Organization organization;
    private User testUser;
    private Blockchain blockchain;
    private List<Replica> replicas;
    private UUID uuid = UUID.fromString("c6b0f366-0884-4ab0-bedf-5fc1ed3c0d56");

    /**
     * initialize the various mocks.
     */
    @BeforeEach
    void init() {
        consortium = new Consortium();
        consortium.setId(CONSORTIUM_ID);
        consortium.setConsortiumName("Consortium Test");
        consortium.setConsortiumType("Test Type");

        organization = new Organization();
        organization.setId(ORG_ID);
        organization.setOrganizationName("Test Org");

        blockchain = Blockchain.builder()
                .consortium(consortium.getId())
                .nodeList(Stream.of("1", "2", "3", "4")
                        .map(s -> new Blockchain.NodeEntry(UUID.randomUUID(), s, s, s, "c".concat(s), null))
                        .collect(Collectors.toList()))
                .build();
        blockchain.setId(uuid);

        replicas = Stream.of("1", "2", "3", "4").map(s -> {
            var r = new Replica();
            r.setPublicIp(s);
            r.setPrivateIp(s);
            r.setUrl(s);
            r.setCert("c".concat(s));
            r.setId(UUID.randomUUID());
            return r;
        }).collect(Collectors.toList());

        // our test user
        testUser = new User();
        testUser.setId(USER_ID);
        testUser.setEmail("user@test.com");
        testUser.setFirstName("Test");
        testUser.setLastName("User");
        testUser.setPassword("1234");
        testUser.setServiceRoles(Collections.singletonList(VmbcRoles.SYSTEM_ADMIN));

        profiles = new DefaultProfiles(userService, organizationService, consortiumService, passwordEncoder,
                                       blockchainService, agreementService, serviceContext, replicaService,
                                       concordService, connectionPoolManager,
                                       ipList, rpcUrls, rpcCerts, true, null);

        when(passwordEncoder.encode(anyString())).then(a -> a.getArguments().toString());
    }

    private void initProfilesExist() {
        when(consortiumService.list()).thenReturn(Arrays.asList(consortium));
        when(organizationService.list()).thenReturn(Arrays.asList(organization));
        when(blockchainService.list()).thenReturn(Arrays.asList(blockchain));

        when(replicaService.getReplicas(blockchain.getId())).thenReturn(replicas);

        when(userService.list()).thenReturn(Arrays.asList(testUser));
        profiles.initialize();
    }

    private void initProfilesEmpty() {
        when(consortiumService.list()).thenReturn(Collections.emptyList());
        when(consortiumService.put(any(Consortium.class))).then(in -> {
            Consortium c = in.getArgument(0);
            c.setId(CONSORTIUM_ID);
            return c;
        });
        when(organizationService.list()).thenReturn(Collections.emptyList());
        when(organizationService.put(any(Organization.class))).then(in -> {
            Organization o = in.getArgument(0);
            o.setId(ORG_ID);
            return o;
        });
        when(blockchainService.list()).thenReturn(Collections.emptyList());
        when(blockchainService.create(any(UUID.class), any(UUID.class), any(), any())).then(in -> {

            Blockchain b = Blockchain.builder()
                    .consortium(in.getArgument(0)).build();
            b.setId(uuid);
            return b;
        });
        when(userService.list()).thenReturn(Collections.emptyList());
        when(userService.put(any(User.class))).thenAnswer(in -> {
            User u = in.getArgument(0);
            u.setId(USER_ID);
            return u;
        });

        when(replicaService.put(any())).then(in -> in.getArgument(0));

        List<Peer> peers = IntStream.range(0, 4)
                .mapToObj(i -> Peer.newBuilder().setHostname("one").setAddress("1:2").build())
                .collect(Collectors.toList());
        when(concordService.getMembers(any(UUID.class))).thenReturn(peers);
        profiles.initialize();
    }

    private List<String> getList(List<Replica> replicas, Function<Replica, String> f) {
        return replicas.stream().map(f).filter(s -> !StringUtils.isEmpty(s)).collect(Collectors.toList());
    }

    @Test
    void testProfileExisting() {
        initProfilesExist();
        List<String> ipList = new ImmutableList.Builder<String>().add("1", "2", "3", "4").build();
        List<String> urlList = new ImmutableList.Builder<String>().add("1", "2", "3", "4").build();
        List<String> certList = new ImmutableList.Builder<String>().add("c1", "c2", "c3", "c4").build();
        Assertions.assertEquals(organization.getId(), profiles.getOrganization().getId());
        Assertions.assertEquals("Test Org", profiles.getOrganization().getOrganizationName());
        Assertions.assertEquals(consortium.getId(), profiles.getConsortium().getId());
        Assertions.assertEquals("Consortium Test", profiles.getConsortium().getConsortiumName());
        Assertions.assertEquals(blockchain.getId(), profiles.getBlockchain().getId());
        Assertions.assertEquals(ipList, getList(profiles.getReplicas(), n -> n.getPrivateIp()));
        Assertions.assertEquals(urlList, getList(profiles.getReplicas(), n -> n.getUrl()));
        Assertions.assertEquals(certList, getList(profiles.getReplicas(), n -> n.getCert()));
        Assertions.assertEquals(testUser.getId(), profiles.getUser().getId());
        Assertions.assertEquals("user@test.com", profiles.getUser().getEmail());
    }

    @Test
    void testProfileExistingNoBlockhain() {
        profiles = new DefaultProfiles(userService, organizationService, consortiumService, passwordEncoder,
                                       blockchainService, agreementService, serviceContext, replicaService,
                                       concordService, connectionPoolManager,
                                       ipList, rpcUrls, rpcCerts, false, null);
        initProfilesExist();
        List<String> ipList = new ImmutableList.Builder<String>().add("1", "2", "3", "4").build();
        List<String> urlList = new ImmutableList.Builder<String>().add("1", "2", "3", "4").build();
        List<String> certList = new ImmutableList.Builder<String>().add("c1", "c2", "c3", "c4").build();
        Assertions.assertEquals(organization.getId(), profiles.getOrganization().getId());
        Assertions.assertEquals("Test Org", profiles.getOrganization().getOrganizationName());
        Assertions.assertEquals(consortium.getId(), profiles.getConsortium().getId());
        Assertions.assertEquals("Consortium Test", profiles.getConsortium().getConsortiumName());
        // If noblockchain is set, all the blockchain fields will be null
        Assertions.assertNull(profiles.getBlockchain().getId());
        Assertions.assertNull(profiles.getBlockchain().getNodeList());
        Assertions.assertNull(profiles.getBlockchain().getConsortium());
        Assertions.assertEquals(testUser.getId(), profiles.getUser().getId());
        Assertions.assertEquals("user@test.com", profiles.getUser().getEmail());
    }

    @Test
    void testProfileEmpty() {
        // This will use the system generated profiles, rather than the ones we created.
        initProfilesEmpty();
        List<String> expectedIps =
                ImmutableList.of("localhost:5458", "localhost:5459", "localhost:5460", "localhost:5461");
        List<String> urlList = new ImmutableList.Builder<String>()
                .add("https://127.0.0.1:8545/")
                .add("https://127.0.0.1:8546/")
                .add("https://127.0.0.1:8547/")
                .add("https://127.0.0.1:8548/").build();
        List<String> certList = ImmutableList.of("/config/replica0-cacert.pem");
        Assertions.assertEquals(organization.getId(), profiles.getOrganization().getId());
        Assertions.assertEquals("ADMIN", profiles.getOrganization().getOrganizationName());
        Assertions.assertEquals(ImmutableMap.of(Constants.ORG_MAX_CHAINS, "0"),
                                profiles.getOrganization().getOrganizationProperties());
        Assertions.assertEquals(consortium.getId(), profiles.getConsortium().getId());
        Assertions.assertEquals("ADMIN", profiles.getConsortium().getConsortiumName());
        Assertions.assertEquals(blockchain.getId(), profiles.getBlockchain().getId());
        Assertions.assertEquals(expectedIps, getList(profiles.getReplicas(), n -> n.getPrivateIp()));
        Assertions.assertEquals(urlList, getList(profiles.getReplicas(), n -> n.getUrl()));
        // Assertions.assertEquals(certList, getList(profiles.getReplicas(), n -> n.getCert()));
        Assertions.assertEquals(testUser.getId(), profiles.getUser().getId());
        Assertions.assertEquals("admin@blockchain.local", profiles.getUser().getEmail());
    }
}
