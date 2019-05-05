/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.db;

import javax.sql.DataSource;

import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.type.TypeHandler;
import org.mybatis.spring.SqlSessionFactoryBean;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;

import com.vmware.blockchain.db.mapper.handlers.UuidTypeHandler;

/**
 * Configuration for entity and link tables.
 */
@Configuration
@MapperScan("com.vmware.blockchain.db")
public class DbConfig {

    private DataSource dataSource;

    @Autowired
    public DbConfig(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    /**
     * TransactionManager for entity table.
     */
    @Bean
    @Primary
    public DataSourceTransactionManager transactionManager() {
        return new DataSourceTransactionManager(dataSource);
    }


    /**
     * SqlSessionFactory bean.
     */
    @Bean
    public SqlSessionFactory sqlSessionFactory() throws Exception {
        SqlSessionFactoryBean sessionFactory = new SqlSessionFactoryBean();
        sessionFactory.setDataSource(dataSource);
        // Setting this in properties file doesn't work.  We seem to have to explicitly set it here.
        TypeHandler<?>[] handlers = { new UuidTypeHandler() };
        sessionFactory.setTypeHandlers(handlers);
        return sessionFactory.getObject();
    }


}
