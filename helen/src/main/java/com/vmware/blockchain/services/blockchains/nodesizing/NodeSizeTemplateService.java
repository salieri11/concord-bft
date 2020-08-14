/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.nodesizing;

import java.util.List;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vmware.blockchain.dao.GenericDao;

/**
 * Class that serves NodeSizeTemplate requests.
 */
@Service
public class NodeSizeTemplateService {
    private final GenericDao genericDao;

    @Autowired
    public NodeSizeTemplateService(GenericDao genericDao) {
        this.genericDao = genericDao;
    }

    /**
     * Get a node sizing template. Currently this method returns a default template.
     * In future this can be modified to support templates at different levels such as Org, Consortium,
     * Blockchain etc.,
     * @return A default template.
     */
    public NodeSizeTemplate getTemplate() {
        List<NodeSizeTemplate> allTemplates = genericDao.getAllByType(NodeSizeTemplate.class);
        return (allTemplates != null && !allTemplates.isEmpty()) ? allTemplates.get(0) : new NodeSizeTemplate();
    }

    /**
     * Get a node size template with supplied id.
     * @param id Id for node template.
     * @return NodeSizeTemplate object that matches the given id.
     */
    public NodeSizeTemplate get(UUID id) {
        return genericDao.get(id, NodeSizeTemplate.class);
    }

    /**
     * Create a new node size template.
     * @param nodeSizeTemplate node size template to be created.
     * @return an instane of node size template.
     */
    public NodeSizeTemplate put(NodeSizeTemplate nodeSizeTemplate) {
        return genericDao.put(nodeSizeTemplate, null);
    }

    /**
     * Delete a Template.
     * @param templateId Template to be deleted.
     */
    public void delete(UUID templateId) {
        genericDao.delete(templateId, NodeSizeTemplate.class);
    }
}
