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
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.vmware.blockchain.auth.AuthHelper;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Controller to handle Consortium.
 */
@RestController
public class ConsortiumController {

    private ConsortiumService consortiumService;
    private AuthHelper authHelper;

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    static class ConGetResponse {
        UUID consortiumId;
        String consortiumName;
    }

    @Data
    static class ConPostBody {
        String consortiumName;
        String consortiumType;
        private UUID organization;
    }

    @Autowired
    public ConsortiumController(ConsortiumService consortiumService, AuthHelper authHelper) {
        this.consortiumService = consortiumService;
        this.authHelper = authHelper;
    }

    /**
     * List all consortium.
     * @return the list of consortium
     */
    @RequestMapping(path = "/api/consortiums", method = RequestMethod.GET)
    @PreAuthorize("isAuthenticated()")
    public ResponseEntity<List<ConGetResponse>> listCons() {
        List<Consortium> orgs = consortiumService.list();
        List<ConGetResponse> rList = orgs.stream().map(c -> new ConGetResponse(c.getId(),
                                                                 c.getConsortiumName())).collect(Collectors.toList());
        return new ResponseEntity<>(rList, HttpStatus.OK);
    }

    /**
     * List all consortiums specific to consortium id.
     * @return specific consortium
     */
    @RequestMapping(path = "/api/consortiums/{con_id}", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.canAccessConsortium(#consortiumId)")
    public ResponseEntity<ConGetResponse> getCon(@PathVariable("con_id") UUID consortiumId) {
        Consortium consortium = consortiumService.get(consortiumId);
        return new ResponseEntity<>(new ConGetResponse(consortium.getId(),
                                                       consortium.getConsortiumName()), HttpStatus.OK);
    }

    /**
     * Creates a new consortium.
     * @param body request body with consortium name
     * @return the new consortium
     */
    @RequestMapping(path = "/api/consortiums", method = RequestMethod.POST)
    @PreAuthorize("hasAnyAuthority(T(com.vmware.blockchain.services.profiles.Roles).consortiumAdmin())")
    public ResponseEntity<ConGetResponse> createCon(@RequestBody ConPostBody body) {
        Consortium consortium = new Consortium(body.getConsortiumName(),
                                               body.getConsortiumType(), body.getOrganization());
        consortium = consortiumService.put(consortium);
        // Need to evict the auth token so we get access to the new consortium
        authHelper.evictToken();
        return new ResponseEntity<>(new ConGetResponse(consortium.getId(),
                                                       consortium.getConsortiumName()), HttpStatus.OK);
    }

    /**
     * Updates a consortium.
     * @param body request body with consortium name
     * @return the updated consortium
     */
    @RequestMapping(path = "/api/consortiums/{con_id}", method = RequestMethod.PATCH)
    @PreAuthorize("@authHelper.canUpdateConsortium(#consortiumId)")
    public ResponseEntity<ConGetResponse> updateCon(@PathVariable("con_id") UUID consortiumId,
                                                    @RequestBody ConPostBody body) {
        Consortium consortium = consortiumService.get(consortiumId);
        if (body.consortiumName != null) {
            consortium.setConsortiumName(body.getConsortiumName());
        }
        consortium = consortiumService.put(consortium);
        return new ResponseEntity<>(new ConGetResponse(consortium.getId(),
                                                       consortium.getConsortiumName()), HttpStatus.OK);
    }
}
