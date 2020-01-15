/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.db.mapper;

import java.util.List;
import java.util.UUID;

import org.apache.ibatis.annotations.Arg;
import org.apache.ibatis.annotations.ConstructorArgs;
import org.apache.ibatis.annotations.Delete;
import org.apache.ibatis.annotations.Insert;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Result;
import org.apache.ibatis.annotations.Results;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.SelectProvider;
import org.apache.ibatis.annotations.Update;
import org.apache.ibatis.type.JdbcType;

import com.vmware.blockchain.db.Entity;
import com.vmware.blockchain.db.mapper.handlers.UuidTypeHandler;
import com.vmware.blockchain.db.mapper.provides.JsonQueryProvider;
import com.vmware.blockchain.db.mapper.provides.ParentListProvider;

/**
 * Mapper for entity table. It will execute sql query on entity and link table depending on method call and return the
 * result.
 */
public interface EntityMapper {

    String SQL_INSERT_ENTITY = "insert into entity(row_key, column_name, version, body, "
            + "user_id, user_name) values(#{rowKey}, #{columnName}, #{version},"
            + "#{body}::JSONB, #{userId}, #{userName})";


    String SQL_INSERT_ENTITY_HISTORY = "insert into entity_history(row_key, column_name, version, body, "
            + "user_id, user_name) values(#{rowKey}, #{columnName}, #{version},"
            + "#{body}::JSONB, #{userId}, #{userName});";

    String SQL_UPDATE_ENTITY = "update entity set "
            + "row_key =#{rowKey}, column_name =#{columnName}, version =#{version}, " + "body =#{body}::JSONB, "
            + "user_id =#{userId}, user_name =#{userName}, created_tms =#{createdTms} "
            + "where version =#{version} - 1 and row_key =#{rowKey} and column_name =#{columnName};";

    String SQL_SELECT_ENTITY = "select * from entity where row_key = #{param1} ";

    String SQL_SELECT_TOP_ENTITY_BY_ROW_KEY =
            "select row_key, column_name, version " + "from entity where row_key = #{param1} limit 1";

    String SQL_SELECT_BY_PARENT = "select e.created_id, e.row_key, e.column_name, e.version, e.body, "
            + "e.user_id, e.user_name, e.created_tms from entity as e " + "join link as l on l.to_row = e.row_key "
            + "where l.from_row = #{param1} and e.row_key = #{param2} and e.column_name = #{param3} ";

    String SQL_GET_HISTORY = "select * from entity_history " + "where row_key = #{param1} and column_name = #{param2} "
            + "order by version desc";

    String SQL_GET_HISTORY_VERSIONS =
            "select * from entity_history " + "where row_key = #{param1} and column_name = #{param2} "
                    + "and version >= #{param3} and version <= #{param4} " + "order by version desc";

    String SQL_GET_BY_PARENT = "select e.created_id, e.row_key, e.column_name, e.version, e.body, "
            + "e.user_id, e.user_name, e.created_tms from entity as e "
            + "join link as l on l.to_row = e.row_key "
            + "where l.from_row = #{param1} and e.column_name = #{param2}";

    String SQL_GET_BY_PARENT_LIST = "select e.created_id, e.row_key, e.column_name, e.version, e.body, "
            + "e.user_id, e.user_name, e.created_tms from entity as e " + "join link as l on l.to_row = e.row_key "
            + "where l.from_row in #{param1} and e.column_name = #{param2}";

    String SQL_GET_BY_TYPE = "select e.created_id, e.row_key, e.column_name, e.version, e.body, "
            + "e.user_id, e.user_name, e.created_tms from entity as e " + " where e.column_name = #{param1}";

    String SQL_GET_BY_TYPE_SUMMARY = "select e.row_key, e.column_name, e.version, " + "e.created_tms from entity as e "
            + "where e.column_name = #{param1}  " + "limit #{param2}";

    String SQL_DELETE_OLDER_VERSIONS = "delete from entity_history "
            + "where row_key = #{param1} and column_name = #{param2} " + "and version < #{param3}";

    String ROLLBACK = "rollback";

    String SQL_COUNT_VERSIONS =
            "select count(*) as row_key from entity_history " + "where row_key = #{param1} and column_name = #{param2}";

    String SQL_DELETE_ENTITY = "delete from entity " + "where row_key = #{param1} and column_name = #{param2}";

    String SQL_DELETE_ENTITY_HISTORY =
            "delete from entity_history " + "where row_key = #{param1} and column_name = #{param2}";

    @ConstructorArgs({
            @Arg(typeHandler = UuidTypeHandler.class, column = "row_key", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class),
            @Arg(typeHandler = UuidTypeHandler.class, column = "user_id", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class)})
    @Insert(SQL_INSERT_ENTITY)
    void saveEntity(Entity entity);

    @ConstructorArgs({
            @Arg(typeHandler = UuidTypeHandler.class, column = "row_key", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class),
            @Arg(typeHandler = UuidTypeHandler.class, column = "user_id", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class)})
    @Insert(SQL_INSERT_ENTITY_HISTORY)
    void saveEntityHistory(Entity entity);

    @ConstructorArgs({
            @Arg(typeHandler = UuidTypeHandler.class, column = "row_key", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class),
            @Arg(typeHandler = UuidTypeHandler.class, column = "user_id", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class)})
    @Update(SQL_UPDATE_ENTITY)
    int updateEntity(Entity entity);

    @Results({@Result(property = "createdId", column = "created_id"),
            @Result(property = "rowKey", column = "row_key", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                    typeHandler = UuidTypeHandler.class),
            @Result(property = "columnName", column = "column_name"),
            @Result(property = "version", column = "version"),
            @Result(property = "body", column = "body"),
            @Result(property = "userId", column = "user_id", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                    typeHandler = UuidTypeHandler.class),
            @Result(property = "userName", column = "user_name"),
            @Result(property = "createdTms", column = "created_tms")})
    @ConstructorArgs({
            @Arg(typeHandler = UuidTypeHandler.class, column = "row_key", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class)})
    @Select(SQL_SELECT_ENTITY)
    Entity getEntityByRowKey(UUID rowKey);

    @Results({
            @Result(property = "rowKey", column = "row_key", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                    typeHandler = UuidTypeHandler.class),
            @Result(property = "columnName", column = "column_name"),
            @Result(property = "version", column = "version")})
    @Select(SQL_SELECT_TOP_ENTITY_BY_ROW_KEY)
    @ConstructorArgs({
            @Arg(typeHandler = UuidTypeHandler.class, column = "row_key", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class)})
    Entity getTopEntityByRowKey(@Param("param1") UUID rowKey);

    @Results({@Result(property = "createdId", column = "created_id"),
            @Result(property = "rowKey", column = "row_key", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                    typeHandler = UuidTypeHandler.class),
            @Result(property = "columnName", column = "column_name"),
            @Result(property = "version", column = "version"),
            @Result(property = "body", column = "body"),
            @Result(property = "userId", column = "user_id", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                    typeHandler = UuidTypeHandler.class),
            @Result(property = "userName", column = "user_name"),
            @Result(property = "createdTms", column = "created_tms")})
    @Select(SQL_SELECT_BY_PARENT)
    @ConstructorArgs({
            @Arg(typeHandler = UuidTypeHandler.class, column = "row_key", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class)})
    Entity getEntityByParent(UUID parentKey, UUID rowKey, String columnName);

    @Results({@Result(property = "createdId", column = "created_id"),
            @Result(property = "rowKey", column = "row_key", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                    typeHandler = UuidTypeHandler.class),
            @Result(property = "columnName", column = "column_name"),
            @Result(property = "version", column = "version"),
            @Result(property = "body", column = "body"),
            @Result(property = "userId", column = "user_id", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                    typeHandler = UuidTypeHandler.class),
            @Result(property = "userName", column = "user_name"),
            @Result(property = "createdTms", column = "created_tms")})
    @Select(SQL_GET_HISTORY)
    @ConstructorArgs({
            @Arg(typeHandler = UuidTypeHandler.class, column = "row_key", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class)})
    List<Entity> getAllHistory(UUID rowKey, String columnName);

    @Results({@Result(property = "createdId", column = "created_id"),
            @Result(property = "rowKey", column = "row_key", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                    typeHandler = UuidTypeHandler.class),
            @Result(property = "columnName", column = "column_name"),
            @Result(property = "version", column = "version"),
            @Result(property = "body", column = "body"),
            @Result(property = "userId", column = "user_id", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                    typeHandler = UuidTypeHandler.class),
            @Result(property = "userName", column = "user_name"),
            @Result(property = "createdTms", column = "created_tms")})
    @Select(SQL_GET_HISTORY_VERSIONS)
    @ConstructorArgs({
            @Arg(typeHandler = UuidTypeHandler.class, column = "row_key", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class)})
    List<Entity> getHistoryVersions(UUID rowKey, String columnName, int start, int end);

    @Results({@Result(property = "createdId", column = "created_id"),
            @Result(property = "rowKey", column = "row_key", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                    typeHandler = UuidTypeHandler.class),
            @Result(property = "columnName", column = "column_name"),
            @Result(property = "version", column = "version"),
            @Result(property = "body", column = "body"),
            @Result(property = "userId", column = "user_id", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                    typeHandler = UuidTypeHandler.class),
            @Result(property = "userName", column = "user_name"),
            @Result(property = "createdTms", column = "created_tms")})
    @Select(SQL_GET_BY_PARENT)
    @ConstructorArgs({
            @Arg(typeHandler = UuidTypeHandler.class, column = "row_key", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class)})
    List<Entity> getByParent(UUID rowKey, String columnName);

    @Results({@Result(property = "createdId", column = "created_id"),
        @Result(property = "rowKey", column = "row_key", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                typeHandler = UuidTypeHandler.class),
        @Result(property = "columnName", column = "column_name"),
        @Result(property = "version", column = "version"),
        @Result(property = "body", column = "body"),
        @Result(property = "userId", column = "user_id", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                typeHandler = UuidTypeHandler.class),
        @Result(property = "userName", column = "user_name"),
        @Result(property = "createdTms", column = "created_tms")})
    @SelectProvider(type = ParentListProvider.class, method = "selectParentList")
    List<Entity> getByParentList(List<UUID> rowKeys, String columnName);

    @Results({@Result(property = "createdId", column = "created_id"),
        @Result(property = "rowKey", column = "row_key", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                typeHandler = UuidTypeHandler.class),
        @Result(property = "columnName", column = "column_name"),
        @Result(property = "version", column = "version"),
        @Result(property = "body", column = "body"),
        @Result(property = "userId", column = "user_id", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                typeHandler = UuidTypeHandler.class),
        @Result(property = "userName", column = "user_name"),
        @Result(property = "createdTms", column = "created_tms")})
    @SelectProvider(type = JsonQueryProvider.class, method = "selectJsonQuery")
    List<Entity> getByJsonQuery(String json, String columnName);

    @Results({@Result(property = "createdId", column = "created_id"),
        @Result(property = "rowKey", column = "row_key", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                typeHandler = UuidTypeHandler.class),
        @Result(property = "columnName", column = "column_name"),
        @Result(property = "version", column = "version"),
        @Result(property = "body", column = "body"),
        @Result(property = "userId", column = "user_id", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                typeHandler = UuidTypeHandler.class),
        @Result(property = "userName", column = "user_name"),
        @Result(property = "createdTms", column = "created_tms")})
    @SelectProvider(type = JsonQueryProvider.class, method = "selectJsonByParentIdQuery")
    @ConstructorArgs({
        @Arg(typeHandler = UuidTypeHandler.class, column = "row_key", jdbcType = JdbcType.OTHER,
                javaType = UUID.class)})
    List<Entity> getJsonByParentQuery(UUID parent, String json, String columnName);

    @Results({@Result(property = "createdId", column = "created_id"),
            @Result(property = "rowKey", column = "row_key", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                    typeHandler = UuidTypeHandler.class),
            @Result(property = "columnName", column = "column_name"),
            @Result(property = "version", column = "version"),
            @Result(property = "body", column = "body"),
            @Result(property = "userId", column = "user_id", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                    typeHandler = UuidTypeHandler.class),
            @Result(property = "userName", column = "user_name"),
            @Result(property = "createdTms", column = "created_tms")})
    @Select(SQL_GET_BY_TYPE)
    List<Entity> getAllByType(String columnName);

    @Results({
            @Result(property = "rowKey", column = "row_key", jdbcType = JdbcType.OTHER, javaType = UUID.class,
                    typeHandler = UuidTypeHandler.class),
            @Result(property = "columnName", column = "column_name"),
            @Result(property = "version", column = "version"),
            @Result(property = "createdTms", column = "created_tms")})
    @Select(SQL_GET_BY_TYPE_SUMMARY)
    List<Entity> getAllByTypeSummary(String columnName, int limit);

    @Delete(SQL_DELETE_OLDER_VERSIONS)
    @ConstructorArgs({
            @Arg(typeHandler = UuidTypeHandler.class, column = "row_key", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class)})
    void deleteOlderVersions(UUID rowKey, String columnName, int version);

    @Update(ROLLBACK)
    @ConstructorArgs({
            @Arg(typeHandler = UuidTypeHandler.class, column = "row_key", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class),
            @Arg(typeHandler = UuidTypeHandler.class, column = "user_id", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class)})
    void rollback();

    @Select(SQL_COUNT_VERSIONS)
    @Results({
            @Result(property = "rowKey", column = "row_key", jdbcType = JdbcType.BIGINT)})
    @ConstructorArgs({
            @Arg(typeHandler = UuidTypeHandler.class, column = "row_key", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class)})
    long countVersions(UUID rowKey, String columnName);

    @Delete(SQL_DELETE_ENTITY)
    @ConstructorArgs({
            @Arg(typeHandler = UuidTypeHandler.class, column = "row_key", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class)})
    void deleteEntity(UUID rowKey, String columnName);

    @Delete(SQL_DELETE_ENTITY_HISTORY)
    @ConstructorArgs({
            @Arg(typeHandler = UuidTypeHandler.class, column = "row_key", jdbcType = JdbcType.OTHER,
                    javaType = UUID.class)})
    void deleteEntityHistory(UUID rowKey, String columnName);
}
