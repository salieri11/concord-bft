/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.db.mapper.provides;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * A mybatis dynamic sql provider to handle selects from parentList.
 */
public class ParentListProvider {

    static final String SQL_GET_BY_PARENT_LIST =
            "select distinct e.created_id, e.row_key, e.column_name, e.version, e.body, "
            + "e.user_id, e.user_name, e.created_tms "
            + "from entity as e "
            + "join link as l on l.to_row = e.row_key "
            + "where l.from_row in (%s) and e.column_name = '%s'";

    /**
     * Returns the query to select from a list of parent ids.
     */
    public String selectParentList(List<UUID> parents, String columnName) {
        String parentStr = parents.stream().map(p -> String.format("'%s'", p.toString()))
                .collect(Collectors.joining(","));
        return String.format(SQL_GET_BY_PARENT_LIST, parentStr, columnName);
    }

}
