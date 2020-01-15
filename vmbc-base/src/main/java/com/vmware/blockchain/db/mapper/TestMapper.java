/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.db.mapper;

import org.apache.ibatis.annotations.Delete;

/**
 * Delete rows from the tables for tests.
 */
public interface TestMapper {
    @Delete("delete from entity")
    void deleteEntity();

    @Delete("delete from entity_history")
    void deleteEntityHistory();

    @Delete("delete from link")
    void deleteLink();


}
