/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.dao;

import java.util.List;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.Function;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.TransactionException;
import org.springframework.transaction.annotation.Transactional;

import com.vmware.blockchain.base.auth.BaseAuthHelper;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.InternalFailureException;


/**
 * Generic DAO implementation.
 *
 * <p>This is a very basic DAO implementation for our schemaless design, the main purpose of this class is to save
 * various entities  into the unified schemaless tables (ENTITY and LINK).
 *
 * <p>Entities need to extend from {@code AbstractEntity}, it should be annotated with {@code EntityColumnName},
 * and the relational Ids should be annotated with {@code LinkedEntityId}, please refer {@code TestEntity} as example.
 *
 * <p>put:
 * Put is one transaction, it either saves the entity, entity_history, and link or fails.
 * It populates rowId if client doesn't supply one, and userId.
 * It check versions
 * It saves/updates relation Ids.
 * It updates the entity, and saves the new version in the entity_history table
 *
 * <p>get/getByParentId/getHistoryVersions
 * those are just straight-forward retrieving data and convert it to java object.
 *
 * <p>All OLTP style methods should be synchronous. Long running queries should be async (and use a separate thread
 * pool)
 *
 */

@Repository
@Transactional
public class GenericDao {

    private static final Logger logger = LogManager.getLogger(GenericDao.class);
    static final int MAX_RETRIES = 3;
    private BaseAuthHelper authHelper;
    private GenericDaoTransaction genericDaoTransaction;

    /**
     * Generic DAO implementation.
     */
    @Autowired
    public GenericDao(GenericDaoTransaction genericDao, BaseAuthHelper authHelper) {
        this.genericDaoTransaction = genericDao;
        this.authHelper = authHelper;
    }

    /**
     * Persist an entity into database under a parent entity.
     *
     * @param newEntity            entity to persist
     * @param currentEntity        previous version entity
     * @param parentId             parent entity Id
     * @return                     persisted entity
     */
    public <E extends AbstractEntity> E putUnderParent(final E newEntity, E currentEntity, UUID parentId) {
        int retries = 0;
        int newEntityVersion = newEntity.getVersion();
        while (true) {
            try {
                return genericDaoTransaction.putUnderParent(newEntity, currentEntity, parentId);
            } catch (TransactionException e) {
                if (retries >= MAX_RETRIES) {
                    logger.warn("Failed to retry for putUnderParent. Max retries exceeded.", e);
                    throw new InternalFailureException(e, ErrorCode.RETRY_FAILURE, "putUnderParent");
                }
                ++retries;
                logger.info("Retrying putUnderParent");
                newEntity.setVersion(newEntityVersion);
            }
        }
    }

    /**
     * Persist an entity into database with the current tenantId.
     *
     * @param newEntity            entity to persist
     * @param currentEntity        previous version entity
     * @return                     persisted entity
     */
    public <E extends AbstractEntity> E putUnderTenant(final E newEntity, E currentEntity) {
        int retries = 0;
        int newEntityVersion = newEntity.getVersion();
        while (true) {
            try {
                return genericDaoTransaction.putUnderParent(newEntity, currentEntity, authHelper.getOrganizationId());
            } catch (TransactionException e) {
                if (retries >= MAX_RETRIES) {
                    logger.warn("Failed to retry for putUnderParent. Max retries exceeded.", e);
                    throw new InternalFailureException(e, ErrorCode.RETRY_FAILURE, "putUnderTenant");
                }
                ++retries;
                logger.info("Retrying putUnderParent");
                newEntity.setVersion(newEntityVersion);
            }
        }
    }



    /**
     * Persist a entity into database.
     *
     * @param newEntity            entity to persist
     * @param currentEntity        previous version entity
     * @return                     persisted entity
     */
    public <E extends AbstractEntity> E put(E newEntity, E currentEntity) {
        return putUnderParent(newEntity, currentEntity, null);
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
    public <E extends AbstractEntity> E mergeWithRetry(E entity, Class<E> entityClass, Consumer<E> merger) {
        return callGenericDaoTransactionWithRetry((m -> m.mergeWithRetry(entity, entityClass, merger)),
                                                  "mergeWithRetry");
    }


    /**
     * Retrieve entity by Id.
     *
     * @param id            entity Id
     * @param entityClass   entity class
     * @return              entity
     */
    public <E extends AbstractEntity> E get(UUID id, Class<E> entityClass) {
        return callGenericDaoTransactionWithRetry((m -> m.get(id, entityClass)), "get");
    }


    /**
     * Retrieve entity by Id, provided the tenant has access.
     * This requires the AuthenticationContext be available.
     *
     * @param id            entity Id
     * @param entityClass   entity class
     * @return              entity
     */
    public <E extends AbstractEntity> E getEntityByTenant(UUID id, Class<E> entityClass) {
        return getEntityByTenant(authHelper.getOrganizationId(), id, entityClass);
    }


    public <E extends AbstractEntity> E getEntityByTenant(UUID tenantId, UUID id, Class<E> entityClass) {
        return callGenericDaoTransactionWithRetry((m -> m.getEntityByTenant(tenantId, id, entityClass)),
                                                  "getEntityByTenant");
    }


    /**
     * Retrieve entity history.
     *
     * @param id            entity Id
     * @param entityClass   entity class
     * @return              list of entities
     */
    public <E extends AbstractEntity> List<E> getHistory(UUID id, Class<E> entityClass) {
        return callGenericDaoTransactionWithRetry((m -> m.getHistory(id, entityClass)), "getHistory");
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
        return callGenericDaoTransactionWithRetry((m -> m.getVersions(id, entityClass, startRange, endRange)),
                                                  "getVersions");

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
        return callGenericDaoTransactionWithRetry((m -> m.getLatestVersions(id, entityClass, versions)),
                                                  "getLatestVersions");
    }


    /**
     * Retrieve entities by parentId.
     *
     * @param parentId      parent Id.
     * @param entityClass   entity class.
     * @return              list entities.
     */
    public <E extends AbstractEntity> List<E> getByParentId(UUID parentId, Class<E> entityClass) {
        return callGenericDaoTransactionWithRetry((m -> m.getByParentId(parentId, entityClass)),
                                                  "getByParentId");
    }

    /**
     * Retrieve entities by parentIds.
     *
     * @param parentIds     List of parent Id.
     * @param entityClass   entity class.
     * @return              list entities.
     */
    public <E extends AbstractEntity> List<E> getByParentList(List<UUID> parentIds, Class<E> entityClass) {
        return callGenericDaoTransactionWithRetry((m -> m.getByParentList(parentIds, entityClass)),
                                                  "getByParentList");
    }

    /**
     * Get an entity using with a json query from the entity body.
     * @param json          A valid json expression
     * @param entityClass   entity class
     * @return              list of entities matching the json
     */
    public <E extends AbstractEntity> List<E> getByJsonQuery(String json, Class<E> entityClass) {
        return callGenericDaoTransactionWithRetry((m -> m.getByJsonQuery(json, entityClass)),
                                                  "getByJsonQuery");
    }

    /**
     * Get an entity using with a json query from the entity body.
     * @param json          A valid json expression
     * @param entityClass   entity class
     * @return              list of entities matching the json
     */
    public <E extends AbstractEntity> List<E> getJsonByParentQuery(UUID parent, String json, Class<E> entityClass) {
        return callGenericDaoTransactionWithRetry((m -> m.getJsonByParentQuery(parent, json, entityClass)),
                                                  "getJsonByParentQuery");
    }

    public <E extends AbstractEntity> List<E> getByTenant(Class<E> entityClass) {
        return getByTenant(entityClass, authHelper.getOrganizationId());
    }


    public <E extends AbstractEntity> List<E> getByTenant(Class<E> entityClass, UUID tenantId) {
        return callGenericDaoTransactionWithRetry((m -> m.getByTenant(entityClass, tenantId)), "getByTenant");
    }



    public <E extends AbstractEntity> List<E> getAllByType(Class<E> entityClass) {
        return callGenericDaoTransactionWithRetry((m -> m.getAllByType(entityClass)), "getAllByType");
    }


    public <E extends AbstractEntity> List<EntitySummary> getAllByTypeSummary(Class<E> entityClass, int limit) {
        return callGenericDaoTransactionWithRetry((m -> m.getAllByTypeSummary(entityClass, limit)),
                                                  "getAllByTypeSummary");
    }


    public <E extends AbstractEntity> void deleteOlderVersions(UUID rowId, Class<E> entityClass, int version) {
        callGenericDaoTransactionWithVoidReturn((m -> m.deleteOlderVersions(rowId, entityClass, version)),
                                                  "deleteOlderVersions");
    }


    public <E extends AbstractEntity> long countVersions(UUID rowId, Class<E> entityClass) {
        return callGenericDaoTransactionWithRetry((m -> m.countVersions(rowId, entityClass)), "countVersions");
    }


    public <E extends AbstractEntity> void delete(UUID rowId, Class<E> entityClass) {
        callGenericDaoTransactionWithVoidReturn((m -> m.delete(rowId, entityClass)), "delete");
    }

    /**
     * Delete a relation link, transaction should be enforced by caller.
     *
     * @param rowId    row Id
     * @param parentId parent Id
     */
    public void deleteRelation(UUID rowId, UUID parentId) {
        callGenericDaoTransactionWithVoidReturn((m -> m.deleteRelation(rowId, parentId)), "deleteRelation");
    }

    /**
     * Save one relation to database, transaction is inherited from caller.
     *
     * @param rowId    row Id
     * @param parentId parent Id
     */
    public void saveRelation(UUID rowId, UUID parentId) {
        callGenericDaoTransactionWithVoidReturn((m -> m.saveRelation(rowId, parentId)), "saveRelation");
    }

    /**
     * Save bidirectional relation to database, transaction is inherited from caller.
     *
     * @param id1 row Id
     * @param id2 parent Id
     */
    public void saveBiDirectionalRelation(UUID id1, UUID id2) {
        callGenericDaoTransactionWithVoidReturn((m -> m.saveBiDirectionalRelation(id1, id2)),
                                                "saveBiDirectionalRelation");
    }

    /**
     * Calls the GenericDaoTransaction method passed with retries.
     * @param action The GenericDaoTransaction method to be called
     * @param methodName The name of the GenericDaoTransaction method with arguments
     * @return Returns the GenericDaoTransaction response
     */
    private <E> E callGenericDaoTransactionWithRetry(Function<GenericDaoTransaction, E> action, String methodName) {
        int retries = 0;
        while (true) {
            try {
                return action.apply(genericDaoTransaction);
            } catch (TransactionException e) {
                if (retries >= MAX_RETRIES) {
                    logger.warn("Failed to retry for {}. Max retries exceeded.", methodName, e);
                    throw new InternalFailureException(e, ErrorCode.RETRY_FAILURE, methodName);
                }
                ++retries;
                logger.info("Retrying {}", methodName);
            }
        }
    }

    /**
     * Calls the GenericDaoTransaction method passed with retries.
     * @param action The GenericDaoTransaction method to be called
     * @param methodName The name of the GenericDaoTransaction method with arguments
     */
    private void callGenericDaoTransactionWithVoidReturn(Consumer<GenericDaoTransaction> action, String methodName) {
        int retries = 0;
        while (true) {
            try {
                action.accept(genericDaoTransaction);
                return;
            } catch (TransactionException e) {
                if (retries >= MAX_RETRIES) {
                    logger.warn("Failed to retry for {}. Max retries exceeded.", methodName, e);
                    throw new InternalFailureException(e, ErrorCode.RETRY_FAILURE, methodName);
                }
                ++retries;
                logger.info("Retrying {}", methodName);
            }
        }
    }
}
