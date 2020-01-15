/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.db;

import java.lang.annotation.Annotation;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.BeanPropertyWriter;
import com.fasterxml.jackson.databind.ser.PropertyWriter;
import com.fasterxml.jackson.databind.ser.impl.SimpleBeanPropertyFilter;
import com.vmware.blockchain.dao.AbstractEntity;

/**
 * Filters the properties while serializing to db.
 */
public class DbEntityPropertyFilter extends SimpleBeanPropertyFilter {

    private static final Logger logger = LogManager.getLogger(DbEntityPropertyFilter.class);

    /**
     * Initializes property filter to be used for DB Entity serialization.
     *
     */
    public DbEntityPropertyFilter() {
        super();
        // Below columns are explicit hence we skip them from serialization.
        serializeAllExcept(AbstractEntity.PROP_ID, AbstractEntity.PROP_USER_ID,
                           AbstractEntity.PROP_CREATED, AbstractEntity.PROP_VERSION);
    }

    @Override
    public void serializeAsField(Object pojo,
                                 JsonGenerator jgen,
                                 SerializerProvider provider,
                                 PropertyWriter writer) throws Exception {
        if (include(writer)) {
            if (isOkToSerialize(writer)) {
                writer.serializeAsField(pojo, jgen, provider);
            } else {
                logger.trace("Exclude property: {}", writer.getName());
            }
        }
    }

    /**
     * Checks if given field should be serialized.
     *
     * @param writer {@link PropertyWriter} associated with the field to be serialized
     * @return true if the field satisfies serialization requirement, false otherwise
     */
    private boolean isOkToSerialize(PropertyWriter writer) {
        DbEntityExclude excludeAll = writer.getAnnotation(DbEntityExclude.class);

        if (excludeAll != null) {
            // Field is annotated to be excluded for all API responses
            traceLog(writer, excludeAll);
            return false;
        }
        // Include the field, since none of the exclusion annotations could be found
        return true;
    }

    private void traceLog(PropertyWriter writer, Annotation annotation) {
        logger.trace("Exclude field: {} annotated with: {} from DB entity.", writer.getName());
    }

    @Override
    protected boolean include(BeanPropertyWriter writer) {
        return true;
    }

    @Override
    protected boolean include(PropertyWriter writer) {
        return true;
    }
}
