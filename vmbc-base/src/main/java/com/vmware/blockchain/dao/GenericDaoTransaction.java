/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.dao;

import java.lang.reflect.Field;
import java.text.MessageFormat;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.ser.FilterProvider;
import com.fasterxml.jackson.databind.ser.impl.SimpleFilterProvider;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.datatype.jsr310.ser.ZonedDateTimeSerializer;
import com.vmware.blockchain.base.auth.BaseAuthHelper;
import com.vmware.blockchain.base.auth.BaseRoles;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.DateTimeUtils;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.InternalFailureException;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.db.DbEntityPropertyFilter;
import com.vmware.blockchain.db.Entity;
import com.vmware.blockchain.db.Link;
import com.vmware.blockchain.db.mapper.EntityMapper;
import com.vmware.blockchain.db.mapper.LinkMapper;

import io.micrometer.core.instrument.Metrics;
import io.micrometer.core.instrument.Timer;

/**
 * Generic DAO transaction implementation. This is separated out so that we can retry on failed transactions exceptions
 * thrown by spring which cannot be caught by the dao layer.
 *
 */

@Repository
class GenericDaoTransaction  {

    private static final Logger logger = LogManager.getLogger(GenericDaoTransaction.class);
    private static final int MAX_RETRIES = 3;
    private static final int LOG_LARGE_RESULTS = 1000;

    private long sqlRetryTimeout;

    // metric constants
    private static final String TAG_ENTITY = "entity";
    private static final String TAG_METHOD = "method";
    private static final String TAG_SERVICE = "service";
    private static final String METRIC_NAME_READ = "dao.read_sec";
    private static final String METRIC_NAME_READ_HISTORY = "dao.read_history_sec";
    private static final String METRIC_NAME_WRITE = "dao.write_sec";
    private static final String METRIC_NAME_WRITE_LINK = "dao.write_link_sec";
    private static final String METRIC_NAME_WRITE_HISTORY = "dao.write_history_sec";

    private static String entityPrefixTag = "bcs";

    private EntityMapper entityMapper;
    private LinkMapper linkMapper;
    private BaseAuthHelper authHelper;

    private ObjectMapper writeMapper = new ObjectMapper();

    private ObjectMapper readIgnorePropertyMapper = new ObjectMapper();

    private Map<Class<? extends AbstractEntity>, List<Field>> linkFieldsMap = new ConcurrentHashMap<>();
    private Map<Class<? extends AbstractEntity>, String> columnNameMap = new ConcurrentHashMap<>();

    /**
     * Generic DAO implementation.
     */
    @Autowired
    public GenericDaoTransaction(EntityMapper entityMapper, LinkMapper linkMapper, BaseAuthHelper authHelper,
            @Value("${dao.sql.retry-timoeout:0}") long sqlRetryTimeout) {
        this.entityMapper = entityMapper;
        this.linkMapper = linkMapper;
        this.authHelper = authHelper;
        if (sqlRetryTimeout == 0) {
            sqlRetryTimeout = TimeUnit.MINUTES.toMillis(5);
        }
        this.sqlRetryTimeout = sqlRetryTimeout;

        /*
         * We are initializing 2 mappers. One is the default for all writes and reads.
         * The other is readIgnorePropertyMapper which gets used if the default mapper
         * fails with unrecognized property exception.
         */
        setGenericMapperProperties(writeMapper);
        setGenericMapperProperties(readIgnorePropertyMapper);
        readIgnorePropertyMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    }

    private void setGenericMapperProperties(ObjectMapper mapperParam) {
        mapperParam.setVisibility(PropertyAccessor.SETTER, JsonAutoDetect.Visibility.NONE);
        mapperParam.setVisibility(PropertyAccessor.GETTER, JsonAutoDetect.Visibility.NONE);
        mapperParam.setVisibility(PropertyAccessor.IS_GETTER, JsonAutoDetect.Visibility.NONE);
        mapperParam.setVisibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY);
        mapperParam.setSerializationInclusion(Include.NON_NULL);
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
                 new ZonedDateTimeSerializer(DateTimeFormatter.ofPattern(DateTimeUtils.ISO_8601_SIMPLE_DATE_PATTERN)));
        mapperParam.registerModule(javaTimeModule);
        mapperParam.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    }

    /**
     * Persist an entity into database under a parent entity.
     *
     * @param newEntity            entity to persist
     * @param currentEntity        previous version entity
     * @param parentId             parent entity Id
     * @return                     persisted entity
     */
    @Transactional(rollbackFor = Exception.class)
    public <E extends AbstractEntity> E putUnderParent(final E newEntity, E currentEntity, UUID parentId) {
        @SuppressWarnings("unchecked")
        Class<E> entityClass = (Class<E>) newEntity.getClass();
        initMeta(entityClass);
        Timer.Sample sample = Timer.start(Metrics.globalRegistry);


        try {

            // Fill in row id if application did not provide one.
            if (newEntity.getId() == null) {
                newEntity.setId(UUID.randomUUID());
            }

            logger.trace("Start saving entity id {}, version {}, columnName {} userName {}, parentId {}",
                         newEntity.getId(), newEntity.getVersion(), getColumnName(entityClass),
                         newEntity.getCreateUserName(), parentId);

            // Check for concurrent update.
            if (newEntity.getVersion() != 0) {
                if (currentEntity == null) {
                    currentEntity = retrieveEntity(newEntity.getId(), entityClass);
                }

                if (currentEntity != null && currentEntity.getVersion() != newEntity.getVersion()) {
                    logger.info("Concurrent Exception Entity {} column name {}", newEntity.getId(),
                            getColumnName(entityClass));
                    throw new ConcurrentUpdateException(ErrorCode.CONCURRENT_UPDATE_FAILED,
                                                        newEntity.getId().toString(), getColumnName(entityClass));
                }
            }

            newEntity.setVersion(newEntity.getVersion() + 1);

            // These fields are only for information purposes and not audit.
            if (newEntity.getCreated() == null) {
                newEntity.setCreated(new Date());
            }
            if (newEntity.getCreateUserId() == null) {
                newEntity.setCreateUserId(authHelper.getUserId());
            }
            if (newEntity.getCreateUserName() == null) {
                newEntity.setCreateUserName(authHelper.getEmail());
            }
            /*
             * This is because insert does not return the updated time stamp generated by the DB.
             */
            if (newEntity.getUpdated() == null) {
                newEntity.setUpdated(new Date());
            }

            newEntity.setUpdatedByUserName(authHelper.getEmail());
            newEntity.setUpdatedByUserId(authHelper.getUserId());
            saveToDatabase(newEntity, entityClass, currentEntity, parentId);
            logger.trace("Finished saving entity id {}, version {}, columnName {} userName {}, parentId {}",
                         newEntity.getId(), newEntity.getVersion(), getColumnName(entityClass),
                         newEntity.getCreateUserName(), parentId);
        } catch (ConcurrentUpdateException e) {
            // re-throw concurrent update exception.
            throw e;
        } catch (Exception e1) {
            // TODO wrap all un-handled exceptions for now, need to fine tune.
            String errMsg =
                    MessageFormat.format("Error occurred during saving entity id {0}, version {1}, columnName {2}"
                                         + " parentId {3}",
                                         newEntity.getId(), newEntity.getVersion(), getColumnName(entityClass),
                                         parentId);
            logger.error(errMsg, e1);
            throw new InternalFailureException(e1, errMsg);
        } finally {
            sample.stop(Metrics.globalRegistry
                                .timer(METRIC_NAME_WRITE, TAG_SERVICE, entityPrefixTag, TAG_ENTITY,
                                       getEntityName(entityClass), TAG_METHOD, "putUnderParent"));
        }
        return newEntity;
    }

    /**
     * Persist a entity into database.
     * This will retry on concurrent update exception - and calls a Consumer to perform the merge.
     * Note that it is the complete responsibility of the Consumer/merger function to appropriately merge -
     * effectively this implements last-writer-wins - so merges are important.
     * We pass in the entity to optimize the common case of no conflicts.
     *
     * @param entity        entity
     * @param entityClass   entity class
     * @param merger        A consumer method that is given the current state of the entire entity and should merge
     *                      in changes.
     * @return              persisted entity
     */
    @Transactional(rollbackFor = Exception.class)
    public <E extends AbstractEntity> E mergeWithRetry(E entity, Class<E> entityClass, Consumer<E> merger) {
        initMeta(entityClass);
        Timer.Sample sample = Timer.start(Metrics.globalRegistry);


        try {
            int retries = MAX_RETRIES;
            while (retries > 0) {
                // Be optimistic - likely there will be no conflict - so no reason to do an additional DB read
                try {
                    merger.accept(entity);
                    return putUnderParent(entity, entity, null);
                } catch (ConcurrentUpdateException e) {
                    // need to call rollback because @Transactional doesn't rollback unless exception is propagated
                    //out of this method.
                    logger.info("Explicitely rolling back transaction after ConcurrentUpdateException.");
                    entityMapper.rollback();
                    retries--;
                    try {
                        // wait a minimal amount of time
                        TimeUnit.MICROSECONDS.sleep(1);
                    } catch (InterruptedException t) {
                        Thread.currentThread().interrupt();
                    }
                    entity = get(entity.getId(), entityClass);
                }
            }
            logger.warn("Failed concurrent update with retries for id {} entity {}", entity.getId(),
                    getColumnName(entityClass));
            throw new ConcurrentUpdateException(ErrorCode.CONCURRENT_UPDATE_FAILED,
                    entity.getId().toString(), getColumnName(entityClass));
        } finally {
            sample.stop(Metrics.globalRegistry
                                .timer(METRIC_NAME_WRITE, TAG_SERVICE, entityPrefixTag, TAG_ENTITY,
                                       getEntityName(entityClass), TAG_METHOD, "mergeWithRetry"));
        }
    }

    /**
     * Retrieve entity by Id.
     *
     * @param id            entity Id
     * @param entityClass   entity class
     * @return              entity
     */
    public <E extends AbstractEntity> E get(UUID id, Class<E> entityClass) {
        initMeta(entityClass);
        final String columnName = getColumnName(entityClass);
        return Timer.builder(METRIC_NAME_READ)
                .tags(TAG_SERVICE, entityPrefixTag, TAG_ENTITY, getEntityName(entityClass), TAG_METHOD, "get")
                .register(Metrics.globalRegistry)
                .record(() -> {
                    logger.trace("Start retrieving entity {}, column name {}.", id, columnName);
                    E entity = retrieveEntity(id, entityClass);
                    if (entity != null) {
                        logger.trace("Finish retrieving entity {}, column name {}.", id, columnName);
                        return entity;
                    }
                    logger.trace("Entity {}, column name {} is not found.", id, columnName);
                    throw new NotFoundException(ErrorCode.ENTITY_NOT_FOUND, id.toString(), columnName);
                });
    }

    /**
     * Retrieve entity by Id, provided the tenant has access.
     * This requires the AuthenticationContext be available.
     *
     * @param id            entity Id
     * @param entityClass   entity class
     * @return              entity
     */
    public <E extends AbstractEntity> E getEntityByTenant(UUID tenantId, UUID id, Class<E> entityClass) {
        initMeta(entityClass);
        return Timer.builder(METRIC_NAME_READ)
                .tags(TAG_SERVICE, entityPrefixTag, TAG_ENTITY, getEntityName(entityClass),
                      TAG_METHOD, "getEntityByTenant")
                .register(Metrics.globalRegistry)
                .record(() -> {
                    logger.trace("Start retrieving entity {}, column name {}, tenant {}.", id,
                                 getColumnName(entityClass),
                                 tenantId);
                    E entity = null;
                    /*
                     * This is critical part of tenant isolation - here we enforce that DB fetches for non-operators
                     * must be linked to tenantId.
                     * For crossOrg style operator calls - we also want to enforce this -
                     * since otherwise a simple mistake of wrong orgId could defeat the goals of crossOrg.
                     */
                    if (tenantId != null && !tenantId.equals(authHelper.getOrganizationId())) {
                        // this really isn't correct - it isn't clear this method should even be exposed.
                        logger.warn("getEntityByType orgId requested {} not equal to authContent {} for column {}",
                                    tenantId, authHelper.getOrganizationId(), getColumnName(entityClass));
                    }
                    if (authHelper.hasAnyAuthority(BaseRoles.systemAdmin())) {
                        // if we are operator, just get the entity
                        entity = retrieveEntity(id, entityClass);
                    } else {
                        // otherwise enforce tenant access
                        entity = retrieveEntityByParent(tenantId, id, entityClass);
                    }
                    if (entity != null) {
                        logger.trace("Finish retrieving entity {}, column name {}, tenant {}.", id,
                                     getColumnName(entityClass),
                                     tenantId);
                        return entity;
                    }
                    logger.trace("Entity {}, column name {}, tenant {} is not found.", id, getColumnName(entityClass),
                                 tenantId);
                    throw new NotFoundException(ErrorCode.ENTITY_NOT_FOUND, id.toString(), getColumnName(entityClass));
                });
    }

    /**
     * Retrieve entity history.
     *
     * @param id            entity Id
     * @param entityClass   entity class
     * @return              list of entities
     */
    public <E extends AbstractEntity> List<E> getHistory(UUID id, Class<E> entityClass) {
        initMeta(entityClass);
        String columnName = getColumnName(entityClass);
        return Timer.builder(METRIC_NAME_READ_HISTORY)
                .tags(TAG_SERVICE, entityPrefixTag, TAG_ENTITY, getEntityName(entityClass), TAG_METHOD, "getHistory")
                .register(Metrics.globalRegistry)
                .record(() -> {
                    logger.trace("Start retrieving entity history for Id {}, column name {}", id, columnName);
                    List<E> result = new ArrayList<>();

                    List<Entity> dbEntities = callEntityMapperWithRetry(
                        m -> m.getAllHistory(id, columnName),
                        String.format("getAllHistory({%s}, {%s})", id, columnName));

                    for (Entity dbEntity : dbEntities) {
                        result.add(dbEntityToEntity(dbEntity, entityClass));
                    }

                    logger.trace("Finish retrieving entity history for Id {}, column name {}", id, columnName);
                    return result;
                });
    }

    /**
     * Retrieve an entity's history in DESC order (within the specified range of version numbers).
     * @param id  entity Id
     * @param entityClass entity classs
     * @param startRange 1st version in range (first if == NULL)
     * @param endRange last version in range (last if == NULL)
     * @return list of entity's versions
     */
    public <E extends AbstractEntity> List<E> getVersions(UUID id, Class<E> entityClass, Integer startRange,
                                                          Integer endRange) {

        if (startRange != null && startRange < 0) {
            throw new BadRequestException(ErrorCode.INVALID_VERSION_PARAM_VAL, String.valueOf(startRange));
        }
        if (endRange != null && endRange < 0) {
            throw new BadRequestException(ErrorCode.INVALID_VERSION_PARAM_VAL, String.valueOf(endRange));
        }

        int start = (startRange == null) ? 0 : startRange;
        int end = (endRange == null) ? Integer.MAX_VALUE : endRange;

        initMeta(entityClass);
        final String columnName = getColumnName(entityClass);
        return Timer.builder(METRIC_NAME_READ_HISTORY)
                .tags(TAG_SERVICE, entityPrefixTag, TAG_ENTITY, getEntityName(entityClass), TAG_METHOD, "getVersions")
                .register(Metrics.globalRegistry)
                .record(() -> {
                    logger.trace("Start retrieving entity history for Id {}, column name {}", id, columnName);
                    List<E> result = new ArrayList<>();

                    List<Entity> dbEntities = callEntityMapperWithRetry(
                        m -> m.getHistoryVersions(id, columnName, start, end),
                        String.format("getHistoryVersions({%s}, {%s}, {%s}, {%s})", id, columnName, start, end));

                    if (dbEntities == null || dbEntities.size() == 0) {
                        throw new NotFoundException(ErrorCode.ENTITY_NOT_FOUND, id.toString(), columnName);
                    }

                    for (Entity dbEntity : dbEntities) {
                        result.add(dbEntityToEntity(dbEntity, entityClass));
                    }
                    logger.trace("Finish retrieving entity history for Id {}, column name {}", id, columnName);
                    return result;
                });
    }

    /**
     * Retrieve an entity's history in DESC order (last requested number of versions).
     * @param id  entity Id
     * @param entityClass entity classs
     * @param versions number of versions to request
     *                 All versions returned if NULL OR no of versions less than requested number
     * @return list of entity's versions
     */
    public <E extends AbstractEntity> List<E> getLatestVersions(UUID id, Class<E> entityClass, Integer versions) {
        initMeta(entityClass);
        String columnName = getColumnName(entityClass);
        logger.trace("Start retrieving entity history for Id {}, column name {}", id, columnName);
        List<E> result = new ArrayList<>();

        List<E> dbEntities = getHistory(id, entityClass);

        //return all versions of Entity if versions = NULL
        int max = versions != null ? versions : dbEntities.size();
        //if max > entity's total versions, return all versions
        for (int i = 0; max > 0 && i < dbEntities.size(); i++, max--) {
            result.add(dbEntities.get(i));
        }
        logger.trace("Finish retrieving entity history for Id {}, column name {}", id, columnName);
        return result;
    }

    /**
     * Retrieve entities by parentId.
     *
     * @param parentId      parent Id.
     * @param entityClass   entity class.
     * @return              list entities.
     */
    public <E extends AbstractEntity> List<E> getByParentId(UUID parentId, Class<E> entityClass) {
        initMeta(entityClass);
        String columnName = getColumnName(entityClass);
        return Timer.builder(METRIC_NAME_READ)
                .tags(TAG_SERVICE, entityPrefixTag, TAG_ENTITY, getEntityName(entityClass), TAG_METHOD, "getByParentId")
                .register(Metrics.globalRegistry)
                .record(() -> {
                    logger.trace("Start retrieving entities for parent {}, column name {}", parentId,
                                 columnName);
                    List<E> result = new ArrayList<>();
                    List<Entity> dbEntities = callEntityMapperWithRetry(
                        m -> m.getByParent(parentId, columnName),
                        String.format("getByParent({%s}, {%s})", parentId, columnName));

                    for (Entity dbEntity : dbEntities) {
                        result.add(dbEntityToEntity(dbEntity, entityClass));
                    }
                    int nDbResults = result.size();

                    logger.trace("Finish retrieving entities for parent {}, column name {}", parentId,
                                 columnName);
                    if (nDbResults > LOG_LARGE_RESULTS) {
                        logger.info("DAO results: getByParentId column {} parent Id {}"
                                    + "  {} db rows returned and {} rows after filtering",
                                    columnName, parentId, nDbResults, result.size());
                    }
                    return result;
                });
    }

    /**
     * Retrieve entities by List of parentIds.
     *
     * @param parentIds     parent Ids.
     * @param entityClass   entity class.
     * @return              list entities.
     */
    public <E extends AbstractEntity> List<E> getByParentList(List<UUID> parentIds, Class<E> entityClass) {
        initMeta(entityClass);
        String columnName = getColumnName(entityClass);
        return Timer.builder(METRIC_NAME_READ).tags(TAG_SERVICE, entityPrefixTag, TAG_ENTITY,
                getEntityName(entityClass), TAG_METHOD, "getByParentList")
                .register(Metrics.globalRegistry)
                .record(() -> {
                    logger.trace("Start retrieving entities for parent {}, column name {}", parentIds,
                                 columnName);
                    List<E> result = new ArrayList<>();
                    List<Entity> dbEntities = callEntityMapperWithRetry(
                        m -> m.getByParentList(parentIds, columnName),
                        String.format("getByParent({%s}, {%s})", parentIds, columnName));

                    for (Entity dbEntity : dbEntities) {
                        result.add(dbEntityToEntity(dbEntity, entityClass));
                    }
                    int nDbResults = result.size();

                    logger.trace("Finish retrieving entities for parent {}, column name {}", parentIds,
                                 columnName);
                    if (nDbResults > LOG_LARGE_RESULTS) {
                        logger.info("DAO results: getByParentId column {} parent Id {}"
                                    + "  {} db rows returned and {} rows after filtering",
                                    columnName, parentIds, nDbResults, result.size());
                    }
                    return result;
                });
    }

    /**
     * Retrieve entities by json terms.
     *
     * @param json          of the form {'tag':'value'}
     * @param entityClass   entity class.
     * @return              list entities.
     */
    public <E extends AbstractEntity> List<E> getByJsonQuery(String json, Class<E> entityClass) {
        initMeta(entityClass);
        String columnName = getColumnName(entityClass);
        return Timer.builder(METRIC_NAME_READ).tags(TAG_SERVICE, entityPrefixTag, TAG_ENTITY,
                getEntityName(entityClass), TAG_METHOD, "getByParentList")
                .register(Metrics.globalRegistry)
                .record(() -> {
                    logger.trace("Start retrieving entities for json {}, column name {}", json,
                                 columnName);
                    List<E> result = new ArrayList<>();
                    List<Entity> dbEntities = callEntityMapperWithRetry(
                        m -> m.getByJsonQuery(json, columnName),
                        String.format("getByJsonQuery({%s}, {%s})", json, columnName));

                    for (Entity dbEntity : dbEntities) {
                        result.add(dbEntityToEntity(dbEntity, entityClass));
                    }
                    int nDbResults = result.size();

                    logger.trace("Finish retrieving entities for josn {}, column name {}", json,
                                 columnName);
                    if (nDbResults > LOG_LARGE_RESULTS) {
                        logger.info("DAO results: getByJsonQuery column {} json {}"
                                    + "  {} db rows returned and {} rows after filtering",
                                    columnName, json, nDbResults, result.size());
                    }
                    return result;
                });
    }

    /**
     * Retrieve entities by json terms.
     *
     * @param json          of the form {'tag':'value'}
     * @param entityClass   entity class.
     * @return              list entities.
     */
    public <E extends AbstractEntity> List<E> getJsonByParentQuery(UUID parent, String json, Class<E> entityClass) {
        initMeta(entityClass);
        String columnName = getColumnName(entityClass);
        return Timer.builder(METRIC_NAME_READ).tags(TAG_SERVICE, entityPrefixTag, TAG_ENTITY,
                getEntityName(entityClass), TAG_METHOD, "getByParentList")
                .register(Metrics.globalRegistry)
                .record(() -> {
                    logger.trace("Start retrieving entities for json {}, column name {}", json,
                                 columnName);
                    List<E> result = new ArrayList<>();
                    List<Entity> dbEntities = callEntityMapperWithRetry(
                        m -> m.getJsonByParentQuery(parent, json, columnName),
                        String.format("getJsonByParentQuery({%s}, {%s}, {%s})", parent, json, columnName));

                    for (Entity dbEntity : dbEntities) {
                        result.add(dbEntityToEntity(dbEntity, entityClass));
                    }
                    int nDbResults = result.size();

                    logger.trace("Finish retrieving entities for josn {}, column name {}", json,
                                 columnName);
                    if (nDbResults > LOG_LARGE_RESULTS) {
                        logger.info("DAO results: getByJsonQuery column {} json {}"
                                    + "  {} db rows returned and {} rows after filtering",
                                    columnName, json, nDbResults, result.size());
                    }
                    return result;
                });
    }

    /**
     * Retrieve entities by tenant.
     *
     * @param entityClass   entity class
     * @return              list entities.
     */
    public <E extends AbstractEntity> List<E> getByTenant(Class<E> entityClass, UUID tenantId) {
        initMeta(entityClass);
        String columnName = getColumnName(entityClass);
        if (tenantId == null) {
            logger.error("Null tenantId for column {}", columnName);
            throw new InternalFailureException(String.format(ErrorCode.NULL_TENANT_ID, columnName));
        }
        return Timer.builder(METRIC_NAME_READ)
                .tags(TAG_SERVICE, entityPrefixTag, TAG_ENTITY, getEntityName(entityClass), TAG_METHOD, "getByTenant")
                .register(Metrics.globalRegistry)
                .record(() -> {
                    logger.trace("Start retrieving entities for tenant {}, column name {}", tenantId, columnName);
                    List<E> result = new ArrayList<>();
                    // if this is our tenant, or we are operator, get the values
                    if ((tenantId.equals(authHelper.getOrganizationId()))
                            || authHelper.hasAnyAuthority(BaseRoles.systemAdmin())) {
                        List<Entity> dbEntities = callEntityMapperWithRetry(m -> m.getByParent(tenantId, columnName),
                                String.format("getByParent({%s}, {%s})", tenantId, columnName));

                        for (Entity dbEntity : dbEntities) {
                            result.add(dbEntityToEntity(dbEntity, entityClass));
                        }
                    }
                    int nDbResults = result.size();
                    logger.trace("Finish retrieving entities for tenant {}, column name {}", tenantId, columnName);
                    if (nDbResults > LOG_LARGE_RESULTS) {
                        logger.info("DAO results: getByTenant column {} parent Id {}"
                                    + "  {} db rows returned and {} rows after filtering",
                                    columnName, tenantId, nDbResults, result.size());
                    }
                    return result;
                });
    }

    /**
     * Retrieve all entities.
     *
     * @param entityClass   entity class
     * @return              list entities.
     */
    public <E extends AbstractEntity> List<E> getAllByType(Class<E> entityClass) {
        initMeta(entityClass);
        String columnName = getColumnName(entityClass);
        return Timer.builder(METRIC_NAME_READ)
                .tags(TAG_SERVICE, entityPrefixTag, TAG_ENTITY, getEntityName(entityClass), TAG_METHOD, "getAllByType")
                .register(Metrics.globalRegistry)
                .record(() -> {
                    logger.info(
                            "Get all entities for column name {}",
                            columnName);
                    List<E> result = new ArrayList<>();

                    List<Entity> dbEntities = callEntityMapperWithRetry(
                        m -> m.getAllByType(columnName),
                        String.format(
                                "getAllByType({%s})", columnName));

                    for (Entity dbEntity : dbEntities) {
                        result.add(dbEntityToEntity(dbEntity, entityClass));
                    }
                    int nDbResults = result.size();

                    logger.trace("Finish retrieving entities for column name {}", columnName);
                    if (nDbResults > LOG_LARGE_RESULTS) {
                        logger.info(
                                "DAO results: getAllByType column {} {} db rows returned and {} rows after filtering",
                                columnName, nDbResults, result.size());
                    }
                    return result;
                });
    }

    /**
     * Retrieve all entities of a type - but just return summary info.
     * @param entityClass entity class
     * @param limit maximum # of rows to return from DB
     * @return list of entity summaries (id, version, updated)
     */
    public <E extends AbstractEntity> List<EntitySummary> getAllByTypeSummary(Class<E> entityClass, int limit) {
        initMeta(entityClass);
        String columnName = getColumnName(entityClass);
        return Timer.builder(METRIC_NAME_READ)
                .tags(TAG_SERVICE, entityPrefixTag, TAG_ENTITY, getEntityName(entityClass),
                      TAG_METHOD, "getAllByTypeSummary")
                .register(Metrics.globalRegistry)
                .record(() -> {
                    List<Entity> dbEntities = callEntityMapperWithRetry(
                        m -> m.getAllByTypeSummary(
                                columnName, limit),
                        String.format(
                                "getAllByType({%s}, limit {%d})",
                                columnName,
                                limit)
                    );
                    logger.trace("Finish retrieving {} entity summaries for column name {}", dbEntities.size(),
                                 columnName);

                    // now convert to EntitySummary
                    List<EntitySummary> summaries = dbEntities.stream()
                            .map(this::convertToEntitySummary).collect(Collectors.toList());
                    return summaries;
                });
    }

    private <E extends AbstractEntity> EntitySummary convertToEntitySummary(Entity dbEntity) {
        return new EntitySummary(dbEntity.getRowKey(), dbEntity.getVersion(),
                                 dbEntity.getCreatedTms());
    }

    /**
     * Delete all versions older than the version passed.
     * @param rowId         row Id
     * @param entityClass   entity class
     * @param version       version number
     */
    @Transactional(rollbackFor = Exception.class)
    public <E extends AbstractEntity> void deleteOlderVersions(UUID rowId, Class<E> entityClass, int version) {
        initMeta(entityClass);
        String columnName = getColumnName(entityClass);
        Timer.builder(METRIC_NAME_WRITE_HISTORY)
                .tags(TAG_SERVICE, entityPrefixTag, TAG_ENTITY, getEntityName(entityClass),
                      TAG_METHOD, "deleteOlderVersions")
                .register(Metrics.globalRegistry)
                .record(() -> {
                    logger.trace("Deleting versions older than {} for entity {}, column name {}.", version, rowId,
                                 columnName);

                    E entity = retrieveEntity(rowId, entityClass);

                    if (entity == null) {
                        logger.trace("Entity {}, column name {} is not found.", rowId, getColumnName(entityClass));
                        throw new BadRequestException(ErrorCode.ENTITY_NOT_FOUND, rowId.toString(),
                                                      getColumnName(entityClass));
                    }

                    if (entity.getVersion() < version) {
                        logger.trace("Cannot delete all versions of entity {}, column name {}.", rowId,
                                     getColumnName(entityClass));
                        throw new BadRequestException(ErrorCode.BAD_REQUEST, rowId.toString(),
                                                      getColumnName(entityClass));
                    }

                    callEntityMapperWithVoidReturn(
                        m -> m.deleteOlderVersions(rowId, columnName, version),
                        String.format("deleteOlderVersions({%s}, {%s}, {%s})", rowId, columnName, version));
                });
    }

    /**
     * Count versions.
     * @param rowId         row Id
     * @param entityClass   entity class
     */
    public <E extends AbstractEntity> long countVersions(UUID rowId, Class<E> entityClass) {
        initMeta(entityClass);
        String columnName = getColumnName(entityClass);

        return Timer.builder(METRIC_NAME_READ_HISTORY)
                .tags(TAG_SERVICE, entityPrefixTag, TAG_ENTITY, getEntityName(entityClass), TAG_METHOD, "countVersions")
                .register(Metrics.globalRegistry)
                .record(() -> {
                    return callEntityMapperWithRetry(
                        m -> m.countVersions(rowId, columnName),
                        String.format("countVersions({%s}, {%s})", rowId, columnName));
                });
    }

    /**
     * Delete entity.
     * @param rowId         row Id
     * @param entityClass   entity class
     */
    @Transactional(rollbackFor = Exception.class)
    public <E extends AbstractEntity> void delete(UUID rowId, Class<E> entityClass) {
        initMeta(entityClass);

        Timer.builder(METRIC_NAME_WRITE)
                .tags(TAG_SERVICE, entityPrefixTag, TAG_ENTITY, getEntityName(entityClass), TAG_METHOD, "delete")
                .register(Metrics.globalRegistry)
                .record(() -> {
                    E entity = retrieveEntity(rowId, entityClass);
                    if (entity == null) {
                        logger.trace("Entity {}, column name {} is not found for deletion.", rowId,
                                     getColumnName(entityClass));
                        throw new NotFoundException(ErrorCode.ENTITY_NOT_FOUND, rowId.toString(),
                                                    getColumnName(entityClass));
                    }

                    if (additionalLinksExist(entity)) {
                        logger.info(
                                "Can not delete entity {}, column name {}. The entity is referenced by other entities.",
                                rowId, getColumnName(entityClass));
                        throw new BadRequestException(ErrorCode.DELETE_INTEGRITY_ERROR,
                                                      rowId.toString(), getColumnName(entityClass));
                    }

                    callLinkMapperWithVoidReturn(
                        l -> l.deleteAllIncomingLinks(rowId),
                        String.format("deleteAllIncomingLinks({%s})", rowId)
                    );

                    callEntityMapperWithVoidReturn(
                        m -> m.deleteEntityHistory(rowId, getColumnName(entityClass)),
                        String.format("deleteEntityHistory({%s}, {%s})", rowId, getColumnName(entityClass))
                    );

                    callEntityMapperWithVoidReturn(
                        m -> m.deleteEntity(rowId, getColumnName(entityClass)),
                        String.format("deleteEntity({%s}, {%s})", rowId, getColumnName(entityClass))
                    );
                });
    }

    /**
     * Check if there are additional links created other than the ones created by the entity itself.
     * @param entity The entity.
     * @return True if additional links exist, else False.
     */
    private <E extends AbstractEntity> boolean additionalLinksExist(E entity) {
        // Outgoing links are created when another entity has referenced this entity.
        List<UUID> outgoingLinks = callLinkMapperWithRetry(
            l -> l.selectToRow(entity.getId()),
            String.format("selectToRow({%s})", entity.getId())
        );

        if (outgoingLinks.size() > 0) {
            logger.info("There are outgoing links from entity {}. Links to {}", entity.getId(),
                        outgoingLinks.stream()
                                .map(s -> s.toString()).collect(Collectors.joining(",")));
            return true;
        }

        ArrayList<UUID> uuidLinks = new ArrayList<>();
        ArrayList<UUID> uuidLinkedFields = new ArrayList<>();

        // When adding an entity, we create incoming links with LinkedEntities to the entity itself. There is
        // however a possibility that more incoming links were created during a separate process.
        List<UUID> incomingLinks = callLinkMapperWithRetry(
            l -> l.selectFromRow(entity.getId()),
            String.format("selectFromRow({%s})", entity.getId())
        );

        if (incomingLinks.size() > linkFieldsMap.get(entity.getClass()).size()) {
            logger.info("There are additional incoming links to entity {}. Links from {}", entity.getId(),
                        incomingLinks.stream()
                                .map(s -> s.toString()).collect(Collectors.joining(",")));
            return true;
        }

        for (UUID link : incomingLinks) {
            uuidLinks.add(link);
        }

        for (Field field : linkFieldsMap.get(entity.getClass())) {
            try {
                uuidLinkedFields.add((UUID) field.get(entity));
            } catch (IllegalAccessException e) {
                logger.error("Failed to cast linked entity field to UUID.", e);
                throw new InternalFailureException(e, ErrorCode.UUID_BINDING_UNSUCCESSFUL, e);
            }
        }

        if (!uuidLinkedFields.containsAll(uuidLinks)) {
            logger.info("There are additional incoming links to entity {}. Links from {}", entity.getId(),
                        incomingLinks.stream()
                                .map(s -> s.toString()).collect(Collectors.joining(",")));
            return true;
        } else {
            return false;
        }
    }

    /**
     * Save entity to database with transaction.
     *
     * @param entity         entity to be saved
     * @param entityClass    entity class
     * @param existingEntity existing entity
     * @throws Exception any exception
     */
    private <E extends AbstractEntity> void saveToDatabase(E entity, Class<E> entityClass, E existingEntity,
                                                           UUID parentId) throws Exception {
        String body = writeMapper.writeValueAsString(entity);
        saveEntity(entity.getId(), body, entityClass, entity.getVersion(), entity.getUpdatedByUserId(),
                   entity.getUpdatedByUserName());
        saveRelations(entity, entityClass, existingEntity);
        if (parentId != null) {
            saveRelation(entity.getId(), parentId);
        }
    }

    /**
     * Save/update relations into database, transaction should be enforced by caller.
     *
     * @param entity         entity
     * @param existingEntity previous version of entity
     * @throws IllegalAccessException Reflection exception.
     */
    private <E extends AbstractEntity> void saveRelations(E entity, Class<E> entityClass, E existingEntity)
            throws IllegalAccessException {
        for (Field field : linkFieldsMap.get(entity.getClass())) {
            UUID val = (UUID) field.get(entity);
            UUID prevVal = null;
            if (existingEntity != null) {
                prevVal = (UUID) field.get(existingEntity);
            }

            if (ObjectUtils.notEqual(val, prevVal)) {
                if (prevVal != null) {
                    deleteRelation(entity.getId(), prevVal);
                    // if bidirectional, delete the other way
                    if (isBidirectional(field)) {
                        deleteRelation(prevVal, entity.getId());
                    }
                }
                if (val != null) {

                    try {
                        if (isBidirectional(field)) {
                            saveBiDirectionalRelation(entity.getId(), val);
                        } else {
                            saveRelation(entity.getId(), val);
                        }
                    } catch (Exception ex) {
                        logger.warn("Save relation failed entity {} column {} prevVal {} val {}",
                                    entity.getId(), getColumnName(entityClass), prevVal, val);
                        throw ex;
                    }
                }
            }
        }
    }

    /**
     * Delete a relation link, transaction should be enforced by caller.
     *
     * @param rowId    row Id
     * @param parentId parent Id
     */
    public void deleteRelation(UUID rowId, UUID parentId) {
        Date startTime = new Date();
        boolean isLogged = false;
        while (true) {
            try {
                linkMapper.deleteLink(parentId, rowId);
                return;
            } catch (Exception e) {
                if (!needToRetry(startTime)) {
                    logger.warn("Failed to retry for deleteRelation({}, {})", rowId, parentId);
                    throw e;
                } else if (!isLogged) {
                    logger.warn("Failed to deleteRelation({}, {}) and retry for error:", rowId, parentId, e);
                    isLogged = true;
                }
            }
        }
    }

    /**
     * Save one relation to database, transaction is inherited from caller.
     *
     * @param rowId    row Id
     * @param parentId parent Id
     */
    public void saveRelation(UUID rowId, UUID parentId) {
        Timer.builder(METRIC_NAME_WRITE_LINK)
                .tags(TAG_SERVICE, entityPrefixTag, TAG_METHOD, "saveRelation")
                .register(Metrics.globalRegistry)
                .record(() -> {
                    // validate that the rowId exists in the DB.
                    getTopEntityByRowKey(rowId);

                    Link dbLink = new Link();
                    dbLink.setFromRow(parentId);
                    dbLink.setToRow(rowId);
                    Date startTime = new Date();
                    boolean isLogged = false;
                    while (true) {
                        try {
                            linkMapper.saveLink(dbLink);
                            return;
                        } catch (DuplicateKeyException e) {
                            // ignore the already exists exception.
                            return;
                        } catch (Exception e) {
                            if (!needToRetry(startTime)) {
                                logger.warn("Failed to retry for saveRelation({}, {})", rowId, parentId);
                                throw e;
                            } else if (!isLogged) {
                                logger.warn("Failed to saveRelation({}, {}) and retry for error:", rowId, parentId, e);
                                isLogged = true;
                            }
                        }
                    }
                });
    }

    /**
     * Save bidirectional relation to database, transaction is inherited from caller.
     *
     * @param id1 row Id
     * @param id2 parent Id
     */
    public void saveBiDirectionalRelation(UUID id1, UUID id2) {
        Timer.builder(METRIC_NAME_WRITE_LINK)
                .tags(TAG_SERVICE, entityPrefixTag, TAG_METHOD, "saveBiDirectionalRelation")
                .register(Metrics.globalRegistry)
                .record(() -> {
                    // validate that the id1 and id2 exists in the DB.
                    getTopEntityByRowKey(id1);
                    getTopEntityByRowKey(id2);

                    Link dbLink = new Link();
                    dbLink.setFromRow(id1);
                    dbLink.setToRow(id2);
                    Date startTime = new Date();
                    boolean isLogged = false;
                    while (true) {
                        try {
                            linkMapper.saveBiDirectionalLink(dbLink);
                            return;
                        } catch (DuplicateKeyException e) {
                            // ignore the already exists exception.
                            return;
                        } catch (Exception e) {
                            if (!needToRetry(startTime)) {
                                logger.warn("Failed to retry for saveBiDirectionalRelation({}, {})", id1, id2);
                                throw e;
                            } else if (!isLogged) {
                                logger.warn("Failed to saveBiDirectionalRelation({}, {}) and retry for error:", id1,
                                            id2, e);
                                isLogged = true;
                            }
                        }
                    }
                });
    }

    /**
     * Fetches the top entity by row_key.
     * @param rowKey The row_key.
     * @return The entity.
     */
    private Entity getTopEntityByRowKey(UUID rowKey) {
        Entity dbEntity = callEntityMapperWithRetry(
            m -> m.getTopEntityByRowKey(rowKey),
            String.format("getTopEntityByRowKey({%s})", rowKey)
        );

        if (dbEntity == null) {
            logger.trace("Entity {} is not found.", rowKey);
            throw new NotFoundException(ErrorCode.ENTITY_NOT_FOUND, rowKey.toString());
        }

        logger.trace("Retreived entity {} from database.", rowKey);
        return dbEntity;
    }

    /**
     * Save one entity to database, transaction should be enforced by caller.
     *
     * @param rowId       row Id
     * @param body        entity body
     * @param entityClass entity class
     * @param newVersion  version
     * @param userId      userId
     */
    private <E extends AbstractEntity> void saveEntity(UUID rowId, String body, Class<E> entityClass,
                                                       int newVersion, UUID userId, String userName) {
        String columnName = getColumnName(entityClass);
        Entity dbEntity = new Entity();
        dbEntity.setBody(body);
        dbEntity.setColumnName(columnName);
        dbEntity.setUserId(userId);
        dbEntity.setUserName(userName);
        dbEntity.setRowKey(rowId);
        dbEntity.setVersion(newVersion);
        dbEntity.setCreatedTms(new Date());
        try {
            saveEntityImp(dbEntity);
        } catch (DuplicateKeyException e) {
            // DO NOT LOG BODY - that can contain secrets.
            logger.info("Entity {}, column name {} has been updated by another client. New version {}, by {}", rowId,
                    columnName, newVersion, userName);
            throw new ConcurrentUpdateException(ErrorCode.CONCURRENT_UPDATE, rowId.toString(),
                    getColumnName(entityClass));
        }
    }

    /**
     * Retrieve one entity from database by row Id.
     *
     * @param rowId row Id
     * @param entityClass entity class
     * @return entity retrieved
     */
    private <E extends AbstractEntity> E retrieveEntity(UUID rowId, Class<E> entityClass) {
        Entity dbEntity = callEntityMapperWithRetry(
            m -> m.getEntityByRowKey(rowId),
            String.format("getEntityByRowKey({%s})", rowId));

        if (dbEntity != null && dbEntity.getColumnName().equals(getColumnName(entityClass))) {
            return dbEntityToEntity(dbEntity, entityClass);
        }
        return null;
    }

    /**
     * Retrieve one entity from database by row Id, provided it links to the parent.
     *
     * @param parentId    parent (tenent) id
     * @param rowId       row Id
     * @param entityClass entity class
     * @return entity retrieved
     */
    private <E extends AbstractEntity> E retrieveEntityByParent(UUID parentId, UUID rowId, Class<E> entityClass) {
        String columnName = getColumnName(entityClass);

        Entity dbEntity = callEntityMapperWithRetry(
            m -> m.getEntityByParent(parentId, rowId, columnName),
            String.format("getEntityByParent({%s}, {%s}, {%s})", parentId, rowId, columnName)
        );

        if (dbEntity != null) {
            return dbEntityToEntity(dbEntity, entityClass);
        }
        return null;
    }

    /**
     * Convert database entity to entity.
     *
     * @param entityClass entity class
     * @param dbEntity    database entity
     * @return entity
     */
    private <E extends AbstractEntity> E dbEntityToEntity(Entity dbEntity, Class<E> entityClass) {
        try {
            logger.trace("dbEntity2Entity source: {}", dbEntity.getBody());
            E entity = readIgnorePropertyMapper.readValue(dbEntity.getBody(), entityClass);

            entity.setId(dbEntity.getRowKey());
            entity.setVersion(dbEntity.getVersion());
            entity.setUpdated(dbEntity.getCreatedTms());
            entity.setUpdatedByUserId(dbEntity.getUserId());
            entity.setUpdatedByUserName(dbEntity.getUserName());
            return entity;
        } catch (Exception e) {
            throw new InternalFailureException(e, ErrorCode.ENTITY_CONVERSION_UNSUCCESSFUL, entityClass.getName());
        }
    }

    /**
     * Retrieve the column name for given entity class.
     *
     * @param entityClass entity class
     * @return column name
     */
    protected <E extends AbstractEntity> String getColumnName(Class<E> entityClass) {
        return columnNameMap.get(entityClass);
    }

    private <E extends AbstractEntity> String getEntityName(Class<E> entityClass) {
        return entityClass.getAnnotation(EntityColumnName.class).value();
    }

    /**
     * Initialize meta data for specified entity class.
     *
     * @param entityClass entity class
     */
    protected <E extends AbstractEntity> void initMeta(Class<E> entityClass) {
        if (!columnNameMap.containsKey(entityClass)) {
            EntityColumnName type = entityClass.getAnnotation(EntityColumnName.class);
            columnNameMap.put(entityClass, type.value());
        }

        if (!linkFieldsMap.containsKey(entityClass)) {
            Field[] fields = entityClass.getDeclaredFields();
            List<Field> relationFields = new ArrayList<>();
            for (Field field : fields) {
                if (field.isAnnotationPresent(LinkedEntityId.class)) {
                    field.setAccessible(true);
                    relationFields.add(field);
                }
            }
            linkFieldsMap.put(entityClass, relationFields);
        }
    }

    private void saveEntityImp(Entity dbEntity) {
        Date startTime = new Date();
        boolean isLogged = false;
        while (true) {
            try {
                if (dbEntity.getVersion() == 1) {
                    entityMapper.saveEntity(dbEntity);
                } else {
                    int updated = entityMapper.updateEntity(dbEntity);
                    if (updated != 1) {
                        throw new DuplicateKeyException(ErrorCode.DUPLICATE_UPDATION);
                    }
                }
                entityMapper.saveEntityHistory(dbEntity);
                return;
            } catch (DuplicateKeyException e1) {
                throw e1;
            } catch (Exception e) {
                e.printStackTrace();
                if (!needToRetry(startTime)) {
                    logger.warn("Failed to retry for Entity {} column {}", dbEntity.getRowKey(),
                                dbEntity.getColumnName());
                    throw e;
                } else if (!isLogged) {
                    logger.warn("Failed to saveEntity for Entity {} column {}. Retrying:",
                                dbEntity.getRowKey(), dbEntity.getColumnName(), e);
                    isLogged = true;
                }
                //need to rollback before trying again, as @Transactional will not handle this case
                logger.info("Retrying saveEntity for Entity {} column {} original exception {}",
                            dbEntity.getRowKey(), dbEntity.getColumnName(), e.toString());
                entityMapper.rollback();
            }
        }
    }

    /**
     * Calls the LinkMapper method passed with retries.
     * @param action The LinkMapper method to be called
     * @param methodName The name of the linkMapper method with arguments
     * @return Returns the LinkMapper response
     */
    private <E> E callLinkMapperWithRetry(Function<LinkMapper, E> action, String methodName) {
        Date startTime = new Date();
        boolean isLogged = false;
        while (true) {
            try {
                return action.apply(linkMapper);
            } catch (Exception e) {
                if (!needToRetry(startTime)) {
                    logger.warn("Failed to retry for {}", methodName);
                    throw e;
                } else if (!isLogged) {
                    logger.warn("Failed to {}", methodName, e);
                    isLogged = true;
                }
                logger.info("Retrying {}", methodName);
            }
        }
    }

    /**
     * Calls the EntityMapper method passed with retries.
     * @param action The EntityMapper method to be called
     * @param methodName The name of the entityMapper method with arguments
     * @return Returns the EntityMapper response
     */
    private <E> E callEntityMapperWithRetry(Function<EntityMapper, E> action, String methodName) {
        Date startTime = new Date();
        boolean isLogged = false;
        while (true) {
            try {
                return action.apply(entityMapper);
            } catch (Exception e) {
                if (!needToRetry(startTime)) {
                    logger.warn("Failed to retry for {}", methodName);
                    throw e;
                } else if (!isLogged) {
                    logger.warn("Failed to {}", methodName, e);
                    isLogged = true;
                }
                logger.info("Retrying {}", methodName);
            }
        }
    }

    /**
     * Calls the LinkMapper methods with void return types.
     * @param action The LinkMapper method to be called
     * @param method The name of the linkMapper method with arguments
     */
    private void callLinkMapperWithVoidReturn(Consumer<LinkMapper> action, String method) {
        Date startTime = new Date();
        boolean isLogged = false;
        while (true) {
            try {
                action.accept(linkMapper);
                return;
            } catch (Exception e) {
                if (!needToRetry(startTime)) {
                    logger.warn("Failed to retry for {}", method);
                    throw e;
                } else if (!isLogged) {
                    logger.warn("Failed to {}", method, e);
                    isLogged = true;
                }
                logger.info("Retrying {}", method);
            }
        }
    }

    /**
     * Calls the EntityMapper methods with void return types.
     * @param action The EntityMapper method to be called
     * @param method The name of the entityMapper method with arguments
     */
    private void callEntityMapperWithVoidReturn(Consumer<EntityMapper> action, String method) {
        Date startTime = new Date();
        boolean isLogged = false;
        while (true) {
            try {
                action.accept(entityMapper);
                return;
            } catch (Exception e) {
                if (!needToRetry(startTime)) {
                    logger.warn("Failed to retry for {}", method);
                    throw e;
                } else if (!isLogged) {
                    logger.warn("Failed to {}", method, e);
                    isLogged = true;
                }
                logger.info("Retrying {}", method);
            }
        }
    }

    private boolean needToRetry(Date startTime) {
        Date currentTime = new Date();
        if (currentTime.getTime() - startTime.getTime() > sqlRetryTimeout) {
            return false;
        }
        try {
            // wait 3 seconds before the retry
            TimeUnit.SECONDS.sleep(3);
        } catch (InterruptedException t) {
            Thread.currentThread().interrupt();
        }
        return true;
    }

    private boolean isBidirectional(Field f) {
        LinkedEntityId link = f.getAnnotation(LinkedEntityId.class);
        return link == null ? false : link.biDirectional();

    }
}
