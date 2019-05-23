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
import org.springframework.web.bind.annotation.RequestBody;
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
    static class OrgGetResponse {
        UUID orgId;
        String organizationName;
    }

    @Data
    static class OrgPostBody {
        String organizationName;
    }

    @Autowired
    public OrganizationContoller(OrganizationService orgService) {
        this.orgService = orgService;
    }

    /**
     * List all orgs.
     * @return list of all orgs
     */
    @RequestMapping(path = "/api/organizations", method = RequestMethod.GET)
    public ResponseEntity<List<OrgGetResponse>> listOrgs() {
        List<Organization> orgs = orgService.list();
        List<OrgGetResponse> rList = orgs.stream().map(o -> new OrgGetResponse(o.getId(), o.getOrganizationName()))
                .collect(Collectors.toList());
        return new ResponseEntity<>(rList, HttpStatus.OK);
    }

    /**
     * List all orgs with id.
     * @return a particular org with id
     */
    @RequestMapping(path = "/api/organizations/{org_id}", method = RequestMethod.GET)
    public ResponseEntity<OrgGetResponse> getOrg(@PathVariable("org_id") UUID orgId) {
        Organization org = orgService.get(orgId);
        return new ResponseEntity<>(new OrgGetResponse(org.getId(), org.getOrganizationName()), HttpStatus.OK);
    }

    /**
     * Creates a new organization.
     * @param body request body with org name
     * @return the new org
     */
    @RequestMapping(path = "/api/organizations", method = RequestMethod.POST)
    public ResponseEntity<OrgGetResponse> createOrg(@RequestBody OrgPostBody body) {
        Organization org = new Organization(body.getOrganizationName());
        org = orgService.put(org);
        return new ResponseEntity<>(new OrgGetResponse(org.getId(), org.getOrganizationName()), HttpStatus.OK);
    }

    /**
     * Updates an organization.
     * @param body request body with org name
     * @return the new org
     */
    @RequestMapping(path = "/api/organizations/{org_id}", method = RequestMethod.PATCH)
    public ResponseEntity<OrgGetResponse> updateOrg(@PathVariable("org_id") UUID orgId, @RequestBody OrgPostBody body) {
        Organization org = orgService.get(orgId);
        if (body.getOrganizationName() != null) {
            org.setOrganizationName(body.getOrganizationName());
        }
        org = orgService.put(org);
        return new ResponseEntity<>(new OrgGetResponse(org.getId(), org.getOrganizationName()), HttpStatus.OK);
    }
}
