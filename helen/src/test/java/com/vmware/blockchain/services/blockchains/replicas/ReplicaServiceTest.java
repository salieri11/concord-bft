/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.replicas;

import static com.vmware.blockchain.services.blockchains.zones.Zone.Type.VMC_AWS;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.dao.TestDaoConfig;
import com.vmware.blockchain.db.DbConfig;
import com.vmware.blockchain.db.mapper.TestMapper;
import com.vmware.blockchain.security.ServiceContext;
import com.vmware.blockchain.services.blockchains.nodesizing.NodeSizeTemplateService;
import com.vmware.blockchain.services.blockchains.zones.Zone;
import com.vmware.blockchain.services.blockchains.zones.ZoneService;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.ProfilesService;

import io.grpc.ManagedChannel;
import io.zonky.test.db.AutoConfigureEmbeddedDatabase;

/**
 * Test the NodeSizeTemplateService.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:db-postgres-test.properties")
@ComponentScan(basePackageClasses = {GenericDao.class, ConsortiumService.class, OrganizationService.class})
@EnableAutoConfiguration
@AutoConfigureEmbeddedDatabase
@ContextConfiguration(classes = {MvcConfig.class, DbConfig.class, TestDaoConfig.class })
public class ReplicaServiceTest {
    private static final UUID USER_ID = UUID.fromString("5df113c4-6f7d-45a1-82f3-806dc6ec6a7e");
    private static final String USER_EMAIL = "test@foo.com";
    private static final UUID DEFAULT_TEMPLATE_ID = UUID.fromString("155de65a-da76-11ea-87d0-0242ac130003");
    private static final String DEFAULT_TEMPLATE_NAME = "Default Template";
    private static final String REPLICA_ID_1 = "1070aacc-6638-4267-8cc9-dbdc07235084";
    private static final String REPLICA_ID_2 = "2198aacc-6638-4267-8cc9-dbdc07235776";
    private static final String BLOCKCHAIN_ID = "fb212e5a-0428-46f9-8faa-b3f15c9843e8";
    private static final String PUBLIC_IP_1 = "34.197.139.214";
    private static final String PUBLIC_IP_2 = "34.197.139.215";
    private static final String PRIVATE_IP_1 = "10.72.217.46";
    private static final String PRIVATE_IP_2 = "10.72.217.47";
    private static final String ZONE_ID = "7eef6110-68bc-11ea-906e-8c859085f3e7";


    @Autowired
    GenericDao genericDao;

    @Autowired
    TestMapper testMapper;

    @MockBean
    private ProfilesService prm;

    @MockBean
    private PasswordEncoder passwordEncoder;

    @MockBean
    private DefaultProfiles profiles;

    @MockBean
    AuthHelper authHelper;

    @MockBean
    ServiceContext serviceContext;

    @MockBean
    @Qualifier("provisioningServerChannel")
    ManagedChannel channel;

    private NodeSizeTemplateService nodeSizeTemplateService;

    private ReplicaService replicaService;
    private ZoneService zoneService;
    private OrganizationService organizationService;

    @BeforeEach
    void setUp() {
        when(authHelper.hasAnyAuthority(anyString())).thenReturn(true);
        //when(authHelper.getOrganizationId()).thenReturn(ONPREM_ORG);
        when(authHelper.getUserId()).thenReturn(USER_ID);
        when(authHelper.getEmail()).thenReturn(USER_EMAIL);

        Replica replica1 = new Replica(PUBLIC_IP_1, PRIVATE_IP_1, "testReplica1",
                                       "https://34.197.139.214:8545", "cert",
                                       UUID.fromString(ZONE_ID), Replica.ReplicaType.READ_REPLICA,
                                       UUID.fromString(BLOCKCHAIN_ID), "password");

        replica1.setId(UUID.fromString(REPLICA_ID_1));

        Replica replica2 =
                new Replica(PUBLIC_IP_2, PRIVATE_IP_2, "testReplica2",
                            "https://34.197.139.215:8545", "cert",
                            UUID.fromString("7eef6110-68bc-11ea-906e-8c859085f3e7"),
                            Replica.ReplicaType.DAML_PARTICIPANT,
                            UUID.fromString(BLOCKCHAIN_ID), "password");

        replica2.setId(UUID.fromString(REPLICA_ID_2));

        organizationService = new OrganizationService(genericDao);

        zoneService = new ZoneService(genericDao, organizationService, authHelper, ZONE_ID.toString());
        Zone zone1 = new Zone(UUID.fromString(ZONE_ID), VMC_AWS, ImmutableMap.of("name", "US_WEST"));
        genericDao.put(zone1, null);

        replicaService = new ReplicaService(genericDao, zoneService);
        genericDao.put(replica1, null);
        genericDao.put(replica2, null);
    }

    @AfterEach
    void cleanup() {
        // Remove any entities we created during setup.
        // If we don't cleanup, we will run into ConcurrentUpdateException, because
        // genericDao.put(..) method is saving the same entity object again and again.
        testMapper.deleteEntity();
        testMapper.deleteEntityHistory();
        testMapper.deleteLink();
    }

    @Test
    public void testGetReplicas() {
        List<Replica> replicas = replicaService.getReplicas(UUID.fromString(BLOCKCHAIN_ID));
        Assertions.assertNotNull(replicas);
        Assertions.assertEquals(2, replicas.size());
    }

    @Test
    public void testGetReplicasByParentId() {
        List<Replica> replicas = replicaService.getReplicasByParentId(UUID.fromString(ZONE_ID));
        Assertions.assertNotNull(replicas);
        Assertions.assertTrue(replicas.size() > 0);
        Assertions.assertEquals(UUID.fromString(BLOCKCHAIN_ID), replicas.get(0).getBlockchainId());
    }

    @Test
    public void testGetReplicasByZoneType() {
        List<Replica> replicas = replicaService.getReplicasByZoneType(Zone.Type.VMC_AWS);
        Assertions.assertNotNull(replicas);
        Assertions.assertEquals(2, replicas.size());
    }
}
