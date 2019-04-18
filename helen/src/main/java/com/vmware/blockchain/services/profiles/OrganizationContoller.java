/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Controller to handle organizations.
 */
@RestController
public class OrganizationContoller {

    private OrganizationService orgService;

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    private static class OrgGetResponse {
        UUID orgId;
        String organizationName;
    }

    @Data
    private static class OrgPostBody {
        String organizationName;
    }

    @Autowired
    public OrganizationContoller(OrganizationService orgService) {
        this.orgService = orgService;
    }

    /**
     * List all orgs.
     * @return
     */
    @RequestMapping(path = "/api/organizations", method = RequestMethod.GET)
    public ResponseEntity<List<OrgGetResponse>> listOrgs() {
        List<Organization> orgs = orgService.list();
        List<OrgGetResponse> rList = orgs.stream().map(o -> new OrgGetResponse(o.getId(), o.getOrganizationName()))
                .collect(Collectors.toList());
        return new ResponseEntity<>(rList, HttpStatus.OK);
    }

    /**
     * List all orgs.
     * @return
     */
    @RequestMapping(path = "/api/organizations/{org_id}", method = RequestMethod.GET)
    public ResponseEntity<OrgGetResponse> getOrg(@PathVariable("org_id") UUID orgId) {
        Organization org = orgService.get(orgId);
        return new ResponseEntity<>(new OrgGetResponse(org.getId(), org.getOrganizationName()), HttpStatus.OK);
    }

}
