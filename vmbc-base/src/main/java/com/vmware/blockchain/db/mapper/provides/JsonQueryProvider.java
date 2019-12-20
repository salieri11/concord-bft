/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.db.mapper.provides;

import java.util.UUID;

/**
 * Mybatis query provider to searc for json terms.
 */
public class JsonQueryProvider {
    static final String SQL_GET_BY_JSON_QUERY =
            "select distinct e.created_id, e.row_key, e.column_name, e.version, e.body, "
            + "e.user_id, e.user_name, e.created_tms "
            + "from entity as e "
            + "where body @> '%s' and column_name = '%s'";

    static final String SQL_GET_JSON_BY_PARENT_QUERY =
            "select e.created_id, e.row_key, e.column_name, e.version, e.body, "
                    + "e.user_id, e.user_name, e.created_tms from entity as e "
                    + "join link as l on l.to_row = e.row_key "
                    + "where l.from_row = '%s' and body @> '%s' and e.column_name = '%s' ";


    /**
     * Returns the query to select from a json query.
     */
    public String selectJsonQuery(String json, String columnName) {
        return String.format(SQL_GET_BY_JSON_QUERY, json, columnName);
    }

    /**
     * Returns the query to select from a json query with a given parent.
     */
    public String selectJsonByParentIdQuery(UUID parent, String json, String columnName) {
        return String.format(SQL_GET_JSON_BY_PARENT_QUERY, parent.toString(), json, columnName);
    }
}
