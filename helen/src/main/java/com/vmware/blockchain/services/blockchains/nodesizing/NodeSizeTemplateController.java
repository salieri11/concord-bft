/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.nodesizing;

import static com.fasterxml.jackson.annotation.JsonTypeInfo.As.EXISTING_PROPERTY;

import java.util.List;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.annotation.JsonTypeInfo;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Rest controller for node sizing API.
 */
@RestController
@RequestMapping(path = "/api/blockchains/nodesizetemplate")
public class NodeSizeTemplateController {

    private final NodeSizeTemplateService nodeSizeTemplateService;

    private static final Logger logger = LogManager.getLogger(NodeSizeTemplateController.class);

    @Autowired
    public NodeSizeTemplateController(NodeSizeTemplateService nodeSizeTemplateService) {
        this.nodeSizeTemplateService = nodeSizeTemplateService;
    }

    /**
     * Response object for nodesizetemplates request.
     */
    @Data
    @NoArgsConstructor
    @JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = EXISTING_PROPERTY, property = "type",
            visible = true, defaultImpl = NodeSizeTemplateController.NodeSizeTemplateResponse.class)
    static class NodeSizeTemplateResponse {
        private UUID id;
        private String name;
        private List<NodeSizeTemplate.Template> templates;
        private NodeSizeTemplate.Range range;

        public NodeSizeTemplateResponse(NodeSizeTemplate nodeSizeTemplate) {
            this.id = nodeSizeTemplate.getId();
            this.name = nodeSizeTemplate.getName();
            this.templates = nodeSizeTemplate.getTemplates();
            this.range = nodeSizeTemplate.getRange();
        }
    }

    /**
     * Get a list of template(s).
     * Any System or Infra or Consortium admins can access this API. This matches with Blockchain POST API.
     * @return Available templates for the organization, if not then return default.
     */
    @RequestMapping(method = RequestMethod.GET)
    @PreAuthorize("@authHelper.isConsortiumAdmin()")
    ResponseEntity<NodeSizeTemplateController.NodeSizeTemplateResponse> getTemplate() {
        NodeSizeTemplate nodeSizeTemplate = nodeSizeTemplateService.getTemplate();
        logger.info("Node size template = " + nodeSizeTemplate);
        NodeSizeTemplateResponse response = new NodeSizeTemplateResponse(nodeSizeTemplate);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }
}
