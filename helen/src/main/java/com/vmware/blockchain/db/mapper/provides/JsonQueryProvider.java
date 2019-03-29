/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.db.mapper.provides;

/**
 * Mybatis query provider to searc for json terms.
 */
public class JsonQueryProvider {
    static final String SQL_GET_BY_JSON_QUERY =
            "select distinct e.created_id, e.row_key, e.column_name, e.version, e.body, "
            + "e.user_id, e.user_name, e.created_tms "
            + "from entity as e "
            + "where body @> '%s' and column_name = '%s'";

    /**
     * Returns the query to select from a list of parent ids.
     */
    public String selectJsonQuery(String json, String columnName) {
        return String.format(SQL_GET_BY_JSON_QUERY, json, columnName);
    }

}
