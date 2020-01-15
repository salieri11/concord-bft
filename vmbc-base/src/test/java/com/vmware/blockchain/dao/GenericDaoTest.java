/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.dao;

import static com.vmware.blockchain.dao.PolyTestEntity.IntType;
import static com.vmware.blockchain.dao.PolyTestEntity.StringType;
import static com.vmware.blockchain.dao.PolyTestEntity.Type;
import static java.time.ZoneOffset.UTC;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.nio.charset.Charset;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutionException;

import org.apache.commons.lang3.ObjectUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.core.io.ClassPathResource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.transaction.TransactionSystemException;
import org.springframework.util.StreamUtils;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.ser.FilterProvider;
import com.fasterxml.jackson.databind.ser.impl.SimpleFilterProvider;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.datatype.jsr310.ser.ZonedDateTimeSerializer;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.base.auth.BaseAuthHelper;
import com.vmware.blockchain.base.auth.BaseRoles;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.DateTimeUtils;
import com.vmware.blockchain.common.InternalFailureException;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.db.DbConfig;
import com.vmware.blockchain.db.DbEntityPropertyFilter;
import com.vmware.blockchain.db.Entity;
import com.vmware.blockchain.db.mapper.EntityMapper;
import com.vmware.blockchain.db.mapper.LinkMapper;
import com.vmware.blockchain.db.mapper.TestMapper;

import io.zonky.test.db.AutoConfigureEmbeddedDatabase;
import net.minidev.json.JSONObject;


/**
 * Test the GenericDao.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:db-postgres-test.properties")
@ComponentScan(basePackageClasses = {GenericDao.class})
@AutoConfigureEmbeddedDatabase
@ContextConfiguration(classes = {DbConfig.class, TestDaoConfig.class })
public class GenericDaoTest {

    @EntityColumnName("test.task")
    private static class Task extends AbstractEntity {
    }

    @Autowired
    private EntityMapper entityMapper;

    @Autowired
    private TestMapper testMapper;

    @Autowired
    private GenericDao genericDao;

    @MockBean
    private BaseAuthHelper authHelper;

    TestEntity parent1;
    TestEntity parent2;
    TestEntity entity1;
    TestEntity entity2;
    TestEntity entity3;
    TestEntity entity4;
    UUID tenantId;
    LocalDateTime ldt;

    UUID userId;

    /**
     * Initialize testing data.
     *
     * @throws ExecutionException   Execution exception from test.
     * @throws InterruptedException Interrupted exception from test.
     */
    @BeforeEach
    void setup() throws InterruptedException, ExecutionException {
        userId = UUID.fromString("7a833e12-6de8-4525-b55e-7cd482f6846e");
        when(authHelper.getUserId()).thenReturn(userId);
        when(authHelper.getEmail()).thenReturn("mockuser");

        tenantId = UUID.randomUUID();
        parent1 = createEntity(null, null, null);
        parent2 = createEntity(null, null, null);
        entity1 = createEntity(null, parent2.getId(), "e1");
        entity2 = createEntity(parent1.getId(), parent2.getId(), "e2");
        entity3 = createEntity(parent1.getId(), parent2.getId(), "e3");
        entity4 = createEntity(null, null, tenantId, "e4");

        genericDao.put(parent1, null);
        genericDao.put(parent2, null);
        genericDao.put(entity1, null);
        genericDao.put(entity2, null);
        genericDao.put(entity3, null);
        genericDao.put(entity4, null);

        // useful LocalDateTime constant
        ldt = LocalDateTime.of(2018, 1, 6, 13, 37, 44, 245000000);
    }

    @AfterEach
    void cleanup() {
        // Remove any entities we created
        testMapper.deleteEntity();
        testMapper.deleteEntityHistory();
        testMapper.deleteLink();
    }

    /**
     * Make sure the contents of one of our entities is what we expect.
     */
    @Test
    void testSimpleGet() throws Exception {
        TestEntity entity = genericDao.get(entity2.getId(), TestEntity.class);
        int age = entity.getAge();
        Assertions.assertEquals(37, age, "message");
        Assertions.assertEquals("This is one test.", entity.getBody());
        double cost = entity.getCost();
        Assertions.assertEquals(41.41, cost);
        Assertions.assertEquals("e2", entity.getOthers());
        Assertions.assertEquals(ZonedDateTime.of(2018, 1, 6, 13, 37, 44, 245000000, ZoneId.of("UTC")),
                entity.getZoneDate());
        Assertions.assertEquals(Instant.parse("2007-12-03T10:15:30.210Z"), entity.getInstant());

    }

    /**
     * Test for simple put.
     */
    @Test
    void testSimplePut() throws Exception {
        try {
            TestEntity entity5 = createEntity(null, null, tenantId, "e5");
            // Test having a null ID
            final UUID oldUuid = entity5.getId();
            entity5.setId(null);

            TestEntity entity = genericDao.put(entity5, null);
            Assertions.assertTrue(compare(entity, entity5));
            Assertions.assertNotNull(entity.getCreated());
            Assertions.assertNotNull(entity.getUpdated());
            Assertions.assertNotNull(entity.getId());
            Assertions.assertNotEquals(oldUuid, entity.getId());
            Assertions.assertEquals(entity.getUpdatedByUserId(), authHelper.getUserId());
            Assertions.assertEquals(entity.getUpdatedByUserName(), authHelper.getEmail());

            entity.setParentId1(parent1.getId());
            TestEntity updatedEntity = genericDao.put(entity, entity);
            Assertions.assertTrue(compare(updatedEntity, entity));
            Assertions.assertNotNull(updatedEntity.getCreated());
            Assertions.assertNotNull(updatedEntity.getUpdated());
            Assertions.assertEquals(updatedEntity.getUpdatedByUserId(), authHelper.getUserId());
            Assertions.assertEquals(updatedEntity.getUpdatedByUserName(), authHelper.getEmail());
            Assertions.assertEquals(2, updatedEntity.getVersion());
            Assertions.assertEquals(2, genericDao.getHistory(entity.getId(),
                                                         TestEntity.class).size());
        } catch (Exception e) {
            fail("Failed to retrieve entity" + e.toString());
        }
    }

    /**
     * Test for testPutUnderParent.
     */
    @Test
    void testPutUnderParent() throws Exception {
        try {
            TestEntity entity6 = createEntity(null, null, tenantId, "e6");
            UUID id = null;

            try {
                id = genericDao.put(entity6, null).getId();
            } catch (NotFoundException e) {
                genericDao.get(entity6.getId(), TestEntity.class);
            }
            TestEntity entity7 = createEntity(null, null, tenantId, "e7");
            genericDao.putUnderParent(entity6, null, entity7.getId());
            List<TestEntity> entities = genericDao.getByParentId(entity7.getId(), TestEntity.class);
            Assertions.assertEquals(1, entities.size());
            Assertions.assertEquals(id, entities.get(0).getId());
        } catch (Exception e) {
            fail("Failed to retrieve entity" + e.toString());
        }
    }

    @Test
    void getByParentList() throws Exception {
        List<UUID> ids = ImmutableList.of(parent1.getId(), parent2.getId());
        List<TestEntity> l = genericDao.getByParentList(ids, TestEntity.class);
        Assertions.assertEquals(3, l.size());
    }

    @Test
    void testInsertThenUpdate() throws Exception {
        EntityMapper mockEntityMapper = mock(EntityMapper.class);
        GenericDaoTransaction mockGenericDaoTransaction =
                new GenericDaoTransaction(mockEntityMapper, mock(LinkMapper.class), authHelper, 0);
        GenericDao mockGenericDao = new GenericDao(mockGenericDaoTransaction, authHelper);
        Entity tenantEntity = new Entity();
        when(mockEntityMapper.getTopEntityByRowKey(any())).thenReturn(tenantEntity);

        TestEntity entity5 = createEntity(null, null, tenantId, "e5");
        mockGenericDao.put(entity5, null);

        ArgumentCaptor<Entity> argument = ArgumentCaptor.forClass(Entity.class);
        verify(mockEntityMapper, times(1)).saveEntity(argument.capture());
        verify(mockEntityMapper, times(0)).updateEntity(argument.capture());
        verify(mockEntityMapper, times(1)).saveEntityHistory(argument.capture());

        reset(mockEntityMapper);
        when(mockEntityMapper.updateEntity(any())).thenReturn(1);
        mockGenericDao.put(entity5, entity5);

        verify(mockEntityMapper, times(0)).saveEntity(argument.capture());
        verify(mockEntityMapper, times(1)).updateEntity(argument.capture());
        verify(mockEntityMapper, times(1)).saveEntityHistory(argument.capture());
    }

    @Test
    void testGetEntityHistory() throws Exception {
        TestEntity entity5 = createEntity(null, null, tenantId, "e5");
        Entity oldEntity = new Entity();
        oldEntity.setVersion(0);
        oldEntity.setColumnName("test.entity");
        oldEntity.setRowKey(entity5.getId());
        oldEntity.setBody(getEntityBody(entity5));
        oldEntity.setUserId(authHelper.getUserId());
        oldEntity.setUserName(authHelper.getEmail());
        entityMapper.saveEntity(oldEntity);
        TestEntity e1 = genericDao.put(entity5, entity5);
        e1.setAge(102);
        genericDao.put(e1, e1);

        List<TestEntity> entityHistory = genericDao.getHistory(e1.getId(), TestEntity.class);
        Assertions.assertEquals(2, entityHistory.size());
        Assertions.assertEquals(2, entityHistory.get(0).getVersion());
        Assertions.assertEquals(1, entityHistory.get(1).getVersion());
        Assertions.assertTrue(compare(e1, entityHistory.get(0)));
    }

    @Test
    void testUpdateWithWrongVersion() throws Exception {
        TestEntity entity5 = createEntity(null, null, tenantId, "e5");
        TestEntity e1 = genericDao.put(entity5, null);
        e1.setAge(130);
        e1.setVersion(2);
        try {
            genericDao.put(e1, entity5);
            fail("Failed to throw exception for updating wrong version");
        } catch (ConcurrentUpdateException e) {
            // threw an ConcurrentUpdateException as expected.
        }
    }

    @Test
    void testSimplePutWithUpdate() throws Exception {
        try {
            TestEntity entity = genericDao.get(entity1.getId(), TestEntity.class);
            Assertions.assertTrue(compare(entity, entity1), "Got different values for stored entity.");
            Assertions.assertEquals(entity.getTenantId(), authHelper.getOrganizationId());

            UUID userId = UUID.randomUUID();
            when(authHelper.getUserId()).thenReturn(userId);
            when(authHelper.getOrganizationId()).thenReturn(userId);
            when(authHelper.getEmail()).thenReturn("mockauthuser1");

            genericDao.put(entity, entity);
            TestEntity updatedEntity = genericDao.get(entity.getId(), TestEntity.class);
            Assertions.assertEquals(entity.getCreated(), updatedEntity.getCreated());
            Assertions.assertEquals(entity.getCreateUserId(), updatedEntity.getCreateUserId());
            Assertions.assertEquals(entity.getCreateUserName(), updatedEntity.getCreateUserName());

            Assertions.assertTrue(entity.getUpdated().before(updatedEntity.getUpdated()));
            Assertions.assertNotEquals(entity.getCreateUserId(), updatedEntity.getUpdatedByUserId());
            Assertions.assertNotEquals(entity.getCreateUserName(), updatedEntity.getUpdatedByUserName());

            Assertions.assertEquals(userId, updatedEntity.getUpdatedByUserId());
            Assertions.assertEquals("mockauthuser1", updatedEntity.getUpdatedByUserName());
            Assertions.assertEquals(2L, genericDao.countVersions(entity.getId(), TestEntity.class));
        } catch (Exception e) {
            fail("Failed to retrieve entity" + e.toString());
        }
    }

    /**
     * Test querying by parent Id.
     */
    @Test
    void testGetByParent() throws Exception {
        try {
            entity1.setAge(105);
            genericDao.put(entity1, entity1);
            List<TestEntity> entities = genericDao.getByParentId(parent1.getId(), TestEntity.class);
            Assertions.assertTrue(entities.size() == 2, "Got different values for stored entity.");

            entities = genericDao.getByParentId(parent2.getId(), TestEntity.class);
            Assertions.assertTrue(entities.size() == 3, "Got different values for stored entity.");
        } catch (Exception e) {
            fail("Failed to retrieve entity" + e.toString());
        }
    }

    /**
     * Test get entity by tenant with success.
     * Use the tenantId we generated in the set up.
     */
    @Test
    void testGetEntityByTenantSuccess() throws Exception {
        try {
            entity4.setAge(105);
            genericDao.put(entity4, entity4);
            TestEntity entity = genericDao.getEntityByTenant(tenantId, entity4.getId(), TestEntity.class);
            Assertions.assertTrue(compare(entity, entity4), "Got different values for stored entity.");
        } catch (Exception e) {
            fail("Failed to retrieve entity" + e.toString());
        }
    }

    /**
     * Test get entity by tenant with success, looking up auth
     * Use the tenantId we generated in the set up.
     */
    @Test
    void testGetEntityByTenantAuthSuccess() throws Exception {
        try {
            when(authHelper.getOrganizationId()).thenReturn(tenantId);
            entity4.setAge(105);
            genericDao.put(entity4, entity4);
            TestEntity entity = genericDao.getEntityByTenant(entity4.getId(), TestEntity.class);
            Assertions.assertTrue(compare(entity, entity4), "Got different values for stored entity.");
            List<TestEntity> l = genericDao.getByTenant(TestEntity.class);
            Assertions.assertEquals(1, l.size());
            Assertions.assertEquals(entity, l.get(0));
        } catch (Exception e) {
            fail("Failed to retrieve entity" + e.toString());
        }
    }

    @Test
    void testGetEntityByTenantAuthOperator() throws Exception {
        try {
            when(authHelper.getOrganizationId()).thenReturn(UUID.randomUUID());
            when(authHelper.hasAnyAuthority(BaseRoles.systemAdmin())).thenReturn(true);
            entity4.setAge(105);
            genericDao.put(entity4, entity4);
            TestEntity entity = genericDao.getEntityByTenant(entity4.getId(), TestEntity.class);
            Assertions.assertTrue(compare(entity, entity4), "Got different values for stored entity.");
            List<TestEntity> l = genericDao.getByTenant(TestEntity.class, tenantId);
            Assertions.assertEquals(1, l.size());
            Assertions.assertEquals(entity, l.get(0));
        } catch (Exception e) {
            fail("Failed to retrieve entity" + e.toString());
        }
    }

    /**
     * Test get entity by tenant with failure.
     * Using the default tenantId should not be able to see entity4
     */
    @Test
    void testGetEntityByTenantFail() throws Exception {
        try {
            when(authHelper.getOrganizationId()).thenReturn(UUID.randomUUID());
            Assertions.assertThrows(NotFoundException.class,
                () -> genericDao.getEntityByTenant(entity4.getId(), TestEntity.class));
            List<TestEntity> l = genericDao.getByTenant(TestEntity.class);
            Assertions.assertTrue(l.isEmpty());
        } catch (Exception e) {
            Assertions.assertTrue(e instanceof NotFoundException, "Did not recieve NotFoundException");
        }
    }

    /**
     * Test get entity without the tenantId.
     * If we just do get, even with a different tenant, we still see the entity
     */
    @Test
    void testGetEntityByTenantOverride() throws Exception {
        try {
            entity4.setAge(105);
            genericDao.put(entity4, entity4);
            TestEntity entity = genericDao.get(entity4.getId(), TestEntity.class);
            Assertions.assertTrue(compare(entity, entity4), "Got different values for stored entity.");
        } catch (Exception e) {
            fail("Failed to retrieve entity" + e.toString());
        }
    }

    /**
     * Test that null orgId, with operator roles works.
     */
    @Test
    void testGetEntityByTenantNoOrgId() throws Exception {
        when(authHelper.getUserId()).thenReturn(userId);
        when(authHelper.getEmail()).thenReturn("mockuser");
        when(authHelper.hasAnyAuthority(any())).thenReturn(true);
        entity4.setAge(105);
        genericDao.put(entity4, entity4);
        TestEntity entity = genericDao.getEntityByTenant(entity4.getId(), TestEntity.class);
        Assertions.assertTrue(compare(entity, entity4), "Got different values for stored entity.");
    }


    /**
     * Test update relations.
     */
    @Test
    void testUpdateRelations() throws Exception {
        try {
            TestEntity entity = genericDao.get(entity2.getId(), TestEntity.class);
            entity.setParentId1(null);
            entity.setParentId2(null);

            genericDao.put(entity, null);

            List<TestEntity> entities = genericDao.getByParentId(parent1.getId(), TestEntity.class);
            Assertions.assertTrue(entities.size() == 1, "Got different values for stored entity.");

            entities = genericDao.getByParentId(parent2.getId(), TestEntity.class);
            Assertions.assertTrue(entities.size() == 2, "Got different values for stored entity.");
        } catch (Exception e) {
            fail("Failed to retrieve entity" + e.toString());
        }
    }

    /**
     * Test get history.
     */
    @Test
    void testGetHistory() throws Exception {
        try {
            TestEntity entity = genericDao.get(entity1.getId(), TestEntity.class);
            entity.setAge(102);
            genericDao.put(entity, entity);

            entity = genericDao.get(entity1.getId(), TestEntity.class);
            entity.setAge(105);
            genericDao.put(entity, entity);

            List<TestEntity> entities = genericDao.getHistory(entity.getId(), TestEntity.class);
            Assertions.assertTrue(entities.size() == 3, "Should have 3 versions in the history after 2 updates.");

        } catch (Exception e) {
            fail("Failed to update/retrieve history." + e.toString());
        }
    }

    /**
     * Test delete older versions with invalid row id.
     */
    @Test
    void testDeleteOlderVersionsWithInvalidRowId() throws Exception {
        try {
            TestEntity entity = genericDao.get(entity1.getId(), TestEntity.class);
            entity.setAge(102);
            genericDao.put(entity, entity);

            genericDao.deleteOlderVersions(UUID.randomUUID(), TestEntity.class, 2);

            fail("Expected BadRequestException for invalid RowId");

        } catch (BadRequestException e) {
            return;
        }
        Assertions.fail();
    }

    /**
     * Test delete older versions with invalid column name.
     */
    @Test
    void testDeleteOlderVersionsWithInvalidColumnName() throws Exception {
        try {
            TestEntity entity = genericDao.get(entity1.getId(), TestEntity.class);
            entity.setAge(102);
            genericDao.put(entity, entity);

            genericDao.deleteOlderVersions(entity.getId(), Task.class, 2);

            fail("Expected BadRequestException for invalid ColumnName");

        } catch (BadRequestException e) {
            return;
        }
        Assertions.fail();
    }

    /**
     * Test delete all versions.
     */
    @Test
    void testDeleteAllVersions() throws Exception {
        TestEntity entity = genericDao.get(entity1.getId(), TestEntity.class);
        entity.setAge(102);
        genericDao.put(entity, entity);

        entity = genericDao.get(entity1.getId(), TestEntity.class);
        entity.setAge(105);
        genericDao.put(entity, entity);

        Assertions.assertThrows(BadRequestException.class,
            () -> genericDao.deleteOlderVersions(entity1.getId(), TestEntity.class, 4));
    }

    /**
     * Test delete older versions.
     */
    @Test
    void testDeleteOlderVersions() throws Exception {
        try {
            TestEntity entity = genericDao.get(entity1.getId(), TestEntity.class);
            entity.setAge(102);
            genericDao.put(entity, entity);

            entity = genericDao.get(entity1.getId(), TestEntity.class);
            entity.setAge(105);
            genericDao.put(entity, entity);

            genericDao.deleteOlderVersions(entity1.getId(), TestEntity.class, 3);

            List<Entity> entities = entityMapper.getAllHistory(entity.getId(),
                                                                   "test.entity");
            Assertions.assertEquals(1, entities.size());
            Assertions.assertEquals(3, entities.get(0).getVersion());

        } catch (Exception e) {
            fail("Failed to delete older versions." + e.toString());
        }
    }

    @Test
    void testGetVersions() throws Exception {
        try {
            TestEntity entity = genericDao.get(entity1.getId(), TestEntity.class);
            entity.setAge(102);
            genericDao.put(entity, entity);

            entity = genericDao.get(entity1.getId(), TestEntity.class);
            entity.setAge(105);

            genericDao.put(entity, entity);

            entity = genericDao.get(entity1.getId(), TestEntity.class);
            entity.setAge(106);

            genericDao.put(entity, entity);

            List<TestEntity> entities = genericDao.getVersions(entity.getId(), TestEntity.class, 2, 3);
            Assertions.assertTrue(entities.size() == 2, "Should return 2 versions of the entity.");
            Assertions.assertTrue(entities.get(0).getVersion() == 3, "first entity should be the 3rd version");
            Assertions.assertTrue(entities.get(1).getVersion() == 2, "second entity should be the 2nd version");

            entities = genericDao.getVersions(entity.getId(), TestEntity.class, null, null);
            Assertions.assertTrue(entities.size() == 4, "Should return all the versions of the entity i.e 4.");

            entities = genericDao.getVersions(entity.getId(), TestEntity.class, 1, 3);
            Assertions.assertTrue(entities.size() == 3, "Should only return versions 1-3 of the entity.");
            Assertions.assertTrue(entities.get(0).getVersion() == 3, "first entity should be the 3rd version");
            Assertions.assertTrue(entities.get(1).getVersion() == 2, "first entity should be the 2nd version");

            entities = genericDao.getVersions(entity.getId(), TestEntity.class, 1, 5);
            Assertions.assertTrue(entities.size() == 4, "Should return versions 1 - 4 (max) of the entity.");
            Assertions.assertTrue(entities.get(0).getVersion() == 4, "first entity should be the 4th version");
            Assertions.assertTrue(entities.get(1).getVersion() == 3, "second entity should be the 3rd version");
            Assertions.assertTrue(entities.get(3).getVersion() == 1, "last entity should be the 1st version");

            entities = genericDao.getVersions(entity.getId(), TestEntity.class, 2, null);
            Assertions.assertTrue(entities.size() == 3, "Should return the last 3 of the entity.");
            Assertions.assertTrue(entities.get(0).getVersion() == 4, "first entity should be the 4th(last) version");
            Assertions.assertTrue(entities.get(1).getVersion() == 3, "second entity should be the 3rd version");
            Assertions.assertTrue(entities.get(2).getVersion() == 2, "third entity should be the 2nd version");

            entities = genericDao.getVersions(entity.getId(), TestEntity.class, null, 2);
            Assertions.assertTrue(entities.get(0).getVersion() == 2, "first entity should be the 2nd version");
            Assertions.assertTrue(entities.get(1).getVersion() == 1, "second entity should be the 1st version");

        } catch (Exception e) {
            fail("Failed to update/retrieve history." + e.toString());
        }
    }

    @Test
    void testGetVersionsWithInvalidRange() throws Exception {
        TestEntity entity = genericDao.get(entity1.getId(), TestEntity.class);
        entity.setAge(102);
        genericDao.put(entity, entity);

        entity = genericDao.get(entity1.getId(), TestEntity.class);
        entity.setAge(105);

        genericDao.put(entity, entity);

        entity = genericDao.get(entity1.getId(), TestEntity.class);
        entity.setAge(106);

        genericDao.put(entity, entity);
        UUID entityId = entity.getId();

        Assertions.assertThrows(NotFoundException.class,
            () -> genericDao.getVersions(entityId, TestEntity.class, 5, 6));

        Assertions.assertThrows(NotFoundException.class,
            () -> genericDao.getVersions(entityId, TestEntity.class, 8, 1));
    }

    @Test
    void testGetLatestVersions() throws Exception {
        try {
            TestEntity entity = genericDao.get(entity1.getId(), TestEntity.class);
            entity.setAge(102);
            genericDao.put(entity, entity);

            entity = genericDao.get(entity1.getId(), TestEntity.class);
            entity.setAge(105);

            genericDao.put(entity, entity);

            List<TestEntity> entities = genericDao.getLatestVersions(entity.getId(), TestEntity.class, 3);
            Assertions.assertTrue(entities.size() == 3, "Should return 3 versions of the entity.");
            Assertions.assertTrue(entities.get(0).getVersion() == 3, "first entity should be the last version");

            entities = genericDao.getLatestVersions(entity.getId(), TestEntity.class, null);
            Assertions.assertTrue(entities.size() == 3, "Should all the versions of the entity i.e 3.");
            Assertions.assertTrue(entities.get(0).getVersion() == 3, "first entity should be the last version");
            Assertions.assertTrue(entities.get(1).getVersion() == 2, "second entity should be the 2nd version");
            Assertions.assertTrue(entities.get(2).getVersion() == 1, "third entity should be the 1st version");

            entities = genericDao.getLatestVersions(entity.getId(), TestEntity.class, 2);
            Assertions.assertTrue(entities.size() == 2, "Should only return last 2 versions of the entity.");
            Assertions.assertTrue(entities.get(1).getVersion() == 2, "second entity should be the 2nd version");

            entities = genericDao.getLatestVersions(entity.getId(), TestEntity.class, 5);
            Assertions.assertTrue(entities.size() == 3, "Should return the max (3) versions of the entity.");

            entities = genericDao.getLatestVersions(entity.getId(), TestEntity.class, -1);
            Assertions.assertTrue(entities.size() == 0, "Should return empty as input is invalid.");

        } catch (Exception e) {
            fail("Failed to update/retrieve history." + e.toString());
        }
    }

    @Test
    void testGetVersionsInvalid() throws Exception {
        TestEntity entity = genericDao.get(entity1.getId(), TestEntity.class);
        try {
            genericDao.getVersions(entity.getId(), TestEntity.class, -3, 2);
        } catch (BadRequestException e) {
            try {
                genericDao.getVersions(entity.getId(), TestEntity.class, 1, -2);
            } catch (BadRequestException e2) {
                return;
            }
            fail("Should receive bad request exception");
        }
        fail("Should receive bad request exception");
    }

    /**
     * Test concurrent write conflict.
     *
     * @throws Throwable Exception from test.
     */
    @Test
    void testWriteConflict() throws Throwable {
        TestEntity entity = genericDao.get(entity1.getId(), TestEntity.class);

        TestEntity original = genericDao.get(entity1.getId(), TestEntity.class);

        genericDao.put(entity, null);

        try {
            genericDao.put(original, null);
        } catch (ConcurrentUpdateException e) {
            return;
        }
        fail("Should receive concurrent update exception.");
    }

    @Test
    void testMerge() throws Throwable {
        // test simple merge where update works
        TestEntity entity = genericDao.mergeWithRetry(entity1, TestEntity.class, me -> {
            me.setAge(47);
        });
        Assertions.assertTrue(47 == entity.getAge(), "Age not updated");
    }

    @Test
    void testMergeWithConflict() throws Throwable {
        // test merge where it always conflicts - make sure eventually ConcurrentUpdateException is thrown.
        TestEntity entity = genericDao.get(entity1.getId(), TestEntity.class);

        try {
            genericDao.mergeWithRetry(entity1, TestEntity.class, me -> {
                me.setAge(47);
                // force conflict
                genericDao.put(entity, null);
            });
        } catch (ConcurrentUpdateException e) {
            return;
        }
        fail("Should receive concurrent update exception");
    }

    @Test
    void testPolymorphic() throws Exception {
        PolyTestEntity.StringType stringType = new PolyTestEntity.StringType("This is the test");
        IntType intType = new IntType(42);
        PolyTestEntity entity = new PolyTestEntity();
        stringType = genericDao.put(stringType, null);
        genericDao.put(intType, null);
        genericDao.put(entity, null);
        final UUID stringId = stringType.getId();

        List<PolyTestEntity> l = genericDao.getAllByType(PolyTestEntity.class);
        Assertions.assertEquals(3, l.size());
        for (PolyTestEntity e : l) {
            if (PolyTestEntity.Type.STRING_TYPE.equals(e.getType())) {
                Assertions.assertTrue(e instanceof StringType);
            }
            if (Type.INT_TYPE.equals(e.getType())) {
                Assertions.assertTrue(e instanceof IntType);
            }
            if (e.getType() == null) {
                Assertions.assertTrue(e instanceof PolyTestEntity);
            }
        }
        String s = JSONObject.toJSONString(Collections.singletonMap("type", "STRING_TYPE"));
        l = genericDao.getByJsonQuery(s, PolyTestEntity.class);
        Assertions.assertEquals(1, l.size());
        Assertions.assertTrue(l.get(0) instanceof StringType);
        StringType  s2 = genericDao.get(stringId, StringType.class);
        Assertions.assertTrue(s2 instanceof StringType);
    }

    /**
     * Verify that the body serialization matches what we think it should.
     */
    @Test
    void testSerialization() throws Exception {
        EntityMapper mockEntityMapper = mock(EntityMapper.class);
        GenericDaoTransaction mockGenericDaoTransaction = new GenericDaoTransaction(mockEntityMapper,
                                                            mock(LinkMapper.class), authHelper, 0);
        GenericDao mockGenericDao = new GenericDao(mockGenericDaoTransaction, authHelper);
        Entity tenantEntity = new Entity();
        when(mockEntityMapper.getTopEntityByRowKey(any())).thenReturn(tenantEntity);

        TestEntity entity5 = createEntity(null, null, tenantId, "e5");
        mockGenericDao.put(entity5, null);

        ArgumentCaptor<Entity> argument = ArgumentCaptor.forClass(Entity.class);
        verify(mockEntityMapper, times(1)).saveEntity(argument.capture());
        JsonNode node = new ObjectMapper().readTree(argument.getValue().getBody());
        Assertions.assertEquals("e5", node.get("others").textValue());
        Assertions.assertEquals(37, node.get("age").intValue());
        Assertions.assertEquals(41.41, node.get("cost").doubleValue(), 0.01);
        Assertions.assertEquals("2018-01-06T13:37:44.245Z", node.get("zone_date").textValue());
        Assertions.assertEquals("2007-12-03T10:15:30.210Z", node.get("instant").textValue());

        // verify that 0 microseconds get serialized correctly (i.e. have xxx.000)
        reset(mockEntityMapper);
        entity5.setZoneDate(ZonedDateTime.of(2018, 1, 6, 13, 37, 44, 0, UTC));
        entity5.setInstant(Instant.parse("2007-12-03T10:15:30Z"));
        when(mockEntityMapper.getTopEntityByRowKey(any())).thenReturn(tenantEntity);
        when(mockEntityMapper.updateEntity(any())).thenReturn(1);
        mockGenericDao.put(entity5, null);
        verify(mockEntityMapper, times(1)).updateEntity(argument.capture());
        node = new ObjectMapper().readTree(argument.getValue().getBody());
        Assertions.assertEquals("2018-01-06T13:37:44.000Z", node.get("zone_date").textValue());
        // TODO: for Instant class - this is hard (to override the serializer) and not worth the effort.
        //Assertions.assertEquals("2007-12-03T10:15:30.000Z", node.get("instant").textValue());
        Assertions.assertEquals("2007-12-03T10:15:30Z", node.get("instant").textValue());
    }

    /**
     * Test that body deserialization matches what we expect.
     * This version tests that for old code (EsxHost) that mistakenly had WRITE_DATES_AS_TIMESTAMPS enabled
     * that those records will deserialize correctly (which jackson seems to do!).
     */
    @Test
    void testDeserializationTimestamps() throws Exception {
        UUID euuid = UUID.randomUUID();
        String entityBody = fromFile("TestEntityWithTimestamps.json");
        Entity result = new Entity();
        result.setBody(entityBody);
        result.setColumnName("test.entity");
        result.setRowKey(euuid);
        result.setUserId(euuid);

        EntityMapper mockEntityMapper = mock(EntityMapper.class);
        GenericDaoTransaction mockGenericDaoTransaction = new GenericDaoTransaction(mockEntityMapper,
                mock(LinkMapper.class), authHelper, 0);
        GenericDao mockGenericDao = new GenericDao(mockGenericDaoTransaction, authHelper);

        when(mockEntityMapper.getEntityByRowKey(any())).thenReturn(result);

        TestEntity testEntity = mockGenericDao.get(euuid, TestEntity.class);
        Assertions.assertEquals(37, (long) testEntity.getAge());
        Assertions.assertEquals(ZonedDateTime.ofLocal(ldt, ZoneId.of("UTC"), UTC), testEntity.getZoneDate());
        Assertions.assertEquals(Instant.parse("2007-12-03T10:15:30.00Z"), testEntity.getInstant());
    }

    /**
     * Test that body deserialization matches what we expect.
     * This version tests that timestamps as strings.
     */
    @Test
    void testDeserialization() throws Exception {
        UUID euuid = UUID.randomUUID();
        String entityBody = fromFile("TestEntity.json");
        Entity result = new Entity();
        result.setBody(entityBody);
        result.setColumnName("test.entity");
        result.setRowKey(euuid);
        result.setUserId(euuid);

        EntityMapper mockEntityMapper = mock(EntityMapper.class);
        GenericDaoTransaction mockGenericDaoTransaction = new GenericDaoTransaction(mockEntityMapper,
                mock(LinkMapper.class), authHelper, 0);
        GenericDao mockGenericDao = new GenericDao(mockGenericDaoTransaction, authHelper);

        when(mockEntityMapper.getEntityByRowKey(any())).thenReturn(result);

        TestEntity testEntity = mockGenericDao.get(euuid, TestEntity.class);
        Assertions.assertEquals(37, (long) testEntity.getAge());
        // Alas the DateTime deserializer creates a ZonedDateTime with offset="Z" and zone="UTC" - but none of the
        // constructors do.
        Assertions.assertEquals(ZonedDateTime.ofLocal(ldt, ZoneId.of("UTC"), UTC), testEntity.getZoneDate());
        Assertions.assertEquals(Instant.parse("2007-12-03T10:15:30.210Z"), testEntity.getInstant());
    }

    @Test
    void testSaveRelation_withInvalidRowId() throws Exception {
        TestEntity invalidEntity = createEntity(null, null, null);
        Assertions.assertThrows(NotFoundException.class,
            () -> genericDao.saveRelation(invalidEntity.getId(), entity1.getId()));
    }

    @Test
    void testSaveBidrectional() throws Exception {
        TestEntity t1 = new TestEntity();
        TestEntity t2 = new TestEntity();
        genericDao.put(t1, null);
        genericDao.put(t2, null);
        genericDao.saveBiDirectionalRelation(t1.getId(), t2.getId());
        List<TestEntity> l1 = genericDao.getByParentId(t1.getId(), TestEntity.class);
        List<TestEntity> l2 = genericDao.getByParentId(t2.getId(), TestEntity.class);
        Assertions.assertEquals(1, l1.size());
        Assertions.assertEquals(1, l2.size());
        Assertions.assertEquals(t1.getId(), l2.get(0).getId());
        Assertions.assertEquals(t2.getId(), l1.get(0).getId());
    }

    @Test
    void testSaveBidirectionalRelation_withInvalidRowId() throws Exception {
        TestEntity invalidEntity = createEntity(null, null, null);
        Assertions.assertThrows(NotFoundException.class,
            () -> genericDao.saveBiDirectionalRelation(invalidEntity.getId(), entity1.getId()));
    }

    @Test
    void testSaveBidirectionalRelation_withInvalidParentId() throws Exception {
        TestEntity invalidEntity = createEntity(null, null, null);
        Assertions.assertThrows(NotFoundException.class,
            () -> genericDao.saveBiDirectionalRelation(entity1.getId(), invalidEntity.getId()));
    }

    @Test
    void testBiDirectionLinkAnnotation() throws Exception {
        BiTestEntity bt = new BiTestEntity();
        bt.setTestEntity(entity1.getId());
        genericDao.put(bt, null);
        List<BiTestEntity> l1 = genericDao.getByParentId(entity1.getId(), BiTestEntity.class);
        List<TestEntity> l2 = genericDao.getByParentId(bt.getId(), TestEntity.class);
        Assertions.assertEquals(1, l1.size());
        Assertions.assertEquals(1, l2.size());
        Assertions.assertEquals(bt.getId(), l1.get(0).getId());
        Assertions.assertEquals(entity1.getId(), l2.get(0).getId());
        // now update with the link removed, and make sure it gets deleted
        bt.setTestEntity(null);
        bt = genericDao.put(bt, null);
        l1 = genericDao.getByParentId(entity1.getId(), BiTestEntity.class);
        l2 = genericDao.getByParentId(bt.getId(), TestEntity.class);
        Assertions.assertTrue(l1.isEmpty());
        Assertions.assertTrue(l2.isEmpty());
    }


    @Test
    void testGetAllByType() throws Exception {
        List<TestEntity> entities = genericDao.getAllByType(TestEntity.class);
        Assertions.assertEquals(6, entities.size());
    }

    @Test
    void testGetByJsonQuery() throws Exception {
        String s = JSONObject.toJSONString(Collections.singletonMap("others", "e2"));
        List<TestEntity> l = genericDao.getByJsonQuery(s, TestEntity.class);
        Assertions.assertEquals(1, l.size());
        Assertions.assertEquals(entity2.getId(), l.get(0).getId());
    }

    @Test
    void testGetByMultiJsonQuery() throws Exception {
        String s = JSONObject.toJSONString(ImmutableMap.of("others", "e2", "age", 37));
        List<TestEntity> l = genericDao.getByJsonQuery(s, TestEntity.class);
        Assertions.assertEquals(1, l.size());
        Assertions.assertEquals(entity2.getId(), l.get(0).getId());
        s = JSONObject.toJSONString(ImmutableMap.of("others", "e2", "age", 42));
        l = genericDao.getByJsonQuery(s, TestEntity.class);
        Assertions.assertTrue(l.isEmpty());
    }

    @Test
    void testGetJsonByParentQuery() throws Exception {
        String s = JSONObject.toJSONString(Collections.singletonMap("others", "e1"));
        List<TestEntity> l = genericDao.getJsonByParentQuery(parent2.getId(), s, TestEntity.class);
        Assertions.assertEquals(1, l.size());
        Assertions.assertEquals(entity1.getId(), l.get(0).getId());
        l = genericDao.getJsonByParentQuery(parent1.getId(), s, TestEntity.class);
        Assertions.assertTrue(l.isEmpty());
    }

    @Test
    void testGetAllByTypeSummary() throws Exception {
        List<EntitySummary> entities = genericDao.getAllByTypeSummary(TestEntity.class, 10);
        Assertions.assertEquals(6, entities.size());
        Assertions.assertNotNull(entities.get(0).getUpdated());
        Assertions.assertNotNull(entities.get(0).getId());
    }

    @Test
    void testDelete() throws Exception {
        genericDao.delete(entity1.getId(), TestEntity.class);
        List<TestEntity> history = genericDao.getHistory(entity1.getId(), TestEntity.class);
        Assertions.assertTrue(history.isEmpty());

        Assertions.assertThrows(NotFoundException.class, () -> genericDao.get(entity1.getId(), TestEntity.class));
    }

    @Test
    void deleteWithAdditionalIncomingLinks() throws Exception {
        genericDao.saveRelation(entity1.getId(), parent1.getId());
        genericDao.saveRelation(entity1.getId(), UUID.randomUUID());
        Assertions.assertThrows(BadRequestException.class, () -> genericDao.delete(entity1.getId(), TestEntity.class));
    }

    @Test
    void deleteWithSameLinksDifferentValues() throws Exception {
        genericDao.saveRelation(entity1.getId(), parent1.getId());
        Assertions.assertThrows(BadRequestException.class, () -> genericDao.delete(entity1.getId(), TestEntity.class));
    }

    @Test
    void deleteWithAdditionalOutgoingLinks() throws Exception {
        genericDao.saveRelation(parent1.getId(), entity1.getId());
        Assertions.assertThrows(BadRequestException.class, () -> genericDao.delete(entity1.getId(), TestEntity.class));
    }

    @Test
    void deleteUnknownEntity() throws Exception {
        UUID randomId = UUID.randomUUID();
        Assertions.assertThrows(NotFoundException.class, () -> genericDao.delete(randomId, TestEntity.class));
    }

    @Test
    void testPutWithRetries() throws Exception {
        GenericDaoTransaction mockGenericDaoTransaction = mock(GenericDaoTransaction.class);
        GenericDao mockGenericDao = new GenericDao(mockGenericDaoTransaction, authHelper);
        entity1.setOthers("e5");
        when(mockGenericDaoTransaction.putUnderParent(entity1, entity1, null)).thenThrow(
                new TransactionSystemException("Test Exception"));
        try {
            mockGenericDao.put(entity1, entity1);
        } catch (InternalFailureException ex) {
            verify(mockGenericDaoTransaction, times(4)).putUnderParent(entity1, entity1, null);
            return;
        }
        Assertions.assertTrue(false);
    }

    @Test
    void testGetWithRetries() throws Exception {
        GenericDaoTransaction mockGenericDaoTransaction = mock(GenericDaoTransaction.class);
        GenericDao mockGenericDao = new GenericDao(mockGenericDaoTransaction, authHelper);
        when(mockGenericDaoTransaction.get(entity1.getId(), entity1.getClass())).thenThrow(
                new TransactionSystemException("Test Exception"));
        try {
            mockGenericDao.get(entity1.getId(), entity1.getClass());
        } catch (InternalFailureException ex) {
            verify(mockGenericDaoTransaction, times(4)).get(entity1.getId(), entity1.getClass());
            return;
        }
        Assertions.assertTrue(false);
    }

    @Test
    void testDeleteWithRetries() throws Exception {
        GenericDaoTransaction mockGenericDaoTransaction = mock(GenericDaoTransaction.class);
        GenericDao mockGenericDao = new GenericDao(mockGenericDaoTransaction, authHelper);
        doThrow(new TransactionSystemException("Test Exception")).when(mockGenericDaoTransaction)
                .delete(entity1.getId(), entity1.getClass());
        try {
            mockGenericDao.delete(entity1.getId(), entity1.getClass());
        } catch (InternalFailureException ex) {
            verify(mockGenericDaoTransaction, times(4)).delete(entity1.getId(), entity1.getClass());
            return;
        }
        Assertions.assertTrue(false);
    }

    private TestEntity createEntity(UUID parent1Id, UUID parent2Id, String other) {
        return createEntity(parent1Id, parent2Id, authHelper.getOrganizationId(), other);
    }

    private TestEntity createEntity(UUID parent1Id, UUID parent2Id, UUID tenantId, String other) {
        TestEntity entity = new TestEntity();
        entity.setId(UUID.randomUUID());
        entity.setTenantId(tenantId);
        entity.setParentId1(parent1Id);
        entity.setParentId2(parent2Id);
        entity.setConfigured(new Date());
        entity.setAge(37);
        entity.setBody("This is one test.");
        entity.setCost(41.41);
        entity.setOthers(other);
        entity.setZoneDate(ZonedDateTime.of(2018, 1, 6, 13, 37, 44, 245000000, UTC));
        entity.setInstant(Instant.parse("2007-12-03T10:15:30.210Z"));
        return entity;
    }

    private boolean compare(TestEntity e1, TestEntity e2) {
        return ObjectUtils.compare(e1.getConfigured(), e2.getConfigured()) == 0
               && ObjectUtils.compare(e1.getAge(), e2.getAge()) == 0
               && ObjectUtils.compare(e1.getBody(), e2.getBody()) == 0
               && ObjectUtils.compare(e1.getOthers(), e2.getOthers()) == 0
               && ObjectUtils.compare(e1.getCost(), e2.getCost()) == 0
               && ObjectUtils.compare(e1.getConfigured(), e2.getConfigured()) == 0;
    }

    private String getEntityBody(TestEntity entity) throws Exception {
        ObjectMapper mapperParam = new ObjectMapper();
        mapperParam.setVisibility(PropertyAccessor.SETTER, JsonAutoDetect.Visibility.NONE);
        mapperParam.setVisibility(PropertyAccessor.GETTER, JsonAutoDetect.Visibility.NONE);
        mapperParam.setVisibility(PropertyAccessor.IS_GETTER, JsonAutoDetect.Visibility.NONE);
        mapperParam.setVisibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY);
        mapperParam.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        mapperParam.enable(SerializationFeature.INDENT_OUTPUT);
        // See {@code SensitiveApiEntity} for details on entity serialization to database
        FilterProvider filters = new SimpleFilterProvider()
                .addFilter(AbstractEntity.ENTITY_WRITER_FILTER_NAME, new DbEntityPropertyFilter());
        mapperParam.setFilterProvider(filters);
        PropertyNamingStrategy propertyNamingStrategy = new PropertyNamingStrategy.SnakeCaseStrategy();
        mapperParam.setPropertyNamingStrategy(propertyNamingStrategy);

        // Allow Jackson to seamlessly serialize/deserialize Java date/time types (Instant, LocalTime, etc.)
        // Note that as described here: https://github.com/FasterXML/jackson-datatype-jsr310/issues/39
        // default format leaves off '0' microSeconds. We want consistent serialization.
        JavaTimeModule javaTimeModule = new JavaTimeModule();
        javaTimeModule.addSerializer(ZonedDateTime.class,
                                     new ZonedDateTimeSerializer(
                                             DateTimeFormatter.ofPattern(DateTimeUtils.ISO_8601_SIMPLE_DATE_PATTERN)));
        mapperParam.registerModule(javaTimeModule);
        mapperParam.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        return mapperParam.writeValueAsString(entity);
    }

    private String fromFile(String location) throws IOException {
        ClassPathResource classPathResource = new ClassPathResource(location);
        return StreamUtils.copyToString(classPathResource.getInputStream(), Charset.defaultCharset());
    }

}
