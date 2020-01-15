/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.db.mapper;

import java.util.List;
import java.util.UUID;

import org.apache.ibatis.annotations.Delete;
import org.apache.ibatis.annotations.Insert;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import com.vmware.blockchain.db.Link;


/**
 * Mapper for Link table.
 */
public interface LinkMapper {

    String SQL_INSERT_LINK = "insert into link(from_row, to_row) values "
                             + "(#{fromRow}, #{toRow}) ";

    String SQL_INSERT_BI_LINK = "insert into link(from_row, to_row) values "
            + "(#{fromRow}, #{toRow})"
            + " ,(#{toRow}, #{fromRow}) ";

    String SQL_SELECT_TO_ROW = "select to_row from link where from_row = #{param1}";

    String SQL_SELECT_FROM_ROW = "select from_row from link where to_row = #{param1}";

    String SQL_DELETE_LINK = "delete from link where from_row = #{param1} and to_row = #{param2} ";

    String SQL_DELETE_ALL_INCOMING_LINKS = "delete from link where to_row = #{param1}";

    @Insert(SQL_INSERT_LINK)
    void saveLink(Link link);

    @Insert(SQL_INSERT_BI_LINK)
    void saveBiDirectionalLink(Link link);

    @Delete(SQL_DELETE_LINK)
    void deleteLink(UUID fromRowKey, UUID toRowKey);

    @Select(SQL_SELECT_TO_ROW)
    List<UUID> selectToRow(@Param("param1") UUID fromRowKey);

    @Select(SQL_SELECT_FROM_ROW)
    List<UUID> selectFromRow(@Param("param1") UUID toRowKey);

    @Delete(SQL_DELETE_ALL_INCOMING_LINKS)
    void deleteAllIncomingLinks(@Param("param1") UUID toRowKey);
}
