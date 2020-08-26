/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.NotFoundException;

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
        UUID organizationId;
        String organizationName;
        Map<String, String> organizationProperties;
    }

    @Data
    static class OrgPatchBody {
        Map<String, String> addProperties;
        Map<String, String> deleteProperties;
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
    @PreAuthorize("@authHelper.isSystemAdmin()")
    public ResponseEntity<List<OrgGetResponse>> listOrgs() {
        List<Organization> orgs = orgService.list();
        List<OrgGetResponse> rList = orgs.stream().map(o -> new OrgGetResponse(o.getId(), o.getOrganizationName(),
                o.getOrganizationProperties()))
                .collect(Collectors.toList());
        return new ResponseEntity<>(rList, HttpStatus.OK);
    }

    /**
     * List all orgs with id.
     * @return a particular org with id
     */
    @RequestMapping(path = "/api/organizations/{org_id}", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.canAccessOrg(#organizationId)")
    public ResponseEntity<OrgGetResponse> getOrg(@PathVariable("org_id") UUID organizationId) throws Exception {
        Organization org = safeGetOrganization(organizationId);
        return new ResponseEntity<>(new OrgGetResponse(org.getId(), org.getOrganizationName(),
                org.getOrganizationProperties()), HttpStatus.OK);
    }

    /**
     * Updates an organization.
     * @param body request body with org name
     * @return the new org
     */
    @RequestMapping(path = "/api/organizations/{org_id}", method = RequestMethod.PATCH)
    @PreAuthorize("@authHelper.canUpdateOrg(#organizationId)")
    public ResponseEntity<OrgGetResponse> updateOrg(@PathVariable("org_id") UUID organizationId,
                                                    @RequestBody OrgPatchBody body) throws Exception {
        Organization org = safeGetOrganization(organizationId);

        Map<String, String> addMap = body.getAddProperties();
        Map<String, String> delMap = body.getDeleteProperties();
        Map<String, String> orgMap = org.getOrganizationProperties();

        if (addMap != null) {
            orgMap.putAll(addMap);
        }

        if (delMap != null) {
            for (String property: delMap.keySet()) {
                orgMap.remove(property);
            }
        }

        org = orgService.put(org);
        return new ResponseEntity<>(new OrgGetResponse(org.getId(), org.getOrganizationName(),
                org.getOrganizationProperties()), HttpStatus.OK);
    }

    private Organization safeGetOrganization(UUID organizationId) {
        Organization org;

        try {
            org = orgService.get(organizationId);
            if (org == null) {
                throw new NotFoundException(ErrorCode.ORG_NOT_FOUND, organizationId.toString());
            }
        } catch (NotFoundException e) {
            throw new NotFoundException(ErrorCode.ORG_NOT_FOUND, organizationId.toString());
        }

        return org;
    }
}