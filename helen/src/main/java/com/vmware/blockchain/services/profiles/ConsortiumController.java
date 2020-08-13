/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;

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
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.services.profiles.OrganizationContoller.OrgGetResponse;

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
        UUID organizationId;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    static class ConPatchResponse {
        UUID consortiumId;
        String consortiumName;
        UUID organizationId;
        List<OrgGetResponse> members;
    }

    @Data
    static class ConPostBody {
        @NotBlank(message = "Consortium name cannot be blank.")
        String consortiumName;
        String consortiumType;
        private UUID organization;
    }

    @Data
    static class ConPatchBody {
        String consortiumName;
        String consortiumType;
        List<UUID> orgsToAdd;
        List<UUID> orgsToRemove;
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
    @PreAuthorize("@authHelper.isAuthenticated()")
    public ResponseEntity<List<ConGetResponse>> listCons() {
        List<Consortium> orgs = consortiumService.list();
        List<ConGetResponse> rList = orgs.stream().map(c -> new ConGetResponse(c.getId(),
                                                                               c.getConsortiumName(),
                                                                               c.getOrganization()))
                .collect(Collectors.toList());
        return new ResponseEntity<>(rList, HttpStatus.OK);
    }

    /**
     * List all consortiums specific to consortium id.
     * @return specific consortium
     */
    @RequestMapping(path = "/api/consortiums/{con_id}", method = RequestMethod.GET)
    @PreAuthorize("@authHelper.canAccessConsortium(#consortiumId)")
    public ResponseEntity<ConGetResponse> getCon(@PathVariable("con_id") UUID consortiumId) {
        Consortium consortium = safeGetConsortium(consortiumId);

        return new ResponseEntity<>(new ConGetResponse(consortium.getId(),
                                                       consortium.getConsortiumName(),
                                                       consortium.getOrganization()), HttpStatus.OK);
    }

    /**
     * Creates a new consortium.
     * @param body request body with consortium name
     * @return the new consortium
     */
    @RequestMapping(path = "/api/consortiums", method = RequestMethod.POST)
    @PreAuthorize("@authHelper.isConsortiumAdmin()")
    public ResponseEntity<ConGetResponse> createCon(@Valid @RequestBody ConPostBody body) {
        // VB-994: Able to create consortia with missing and empty names.
        Consortium consortium = new Consortium(body.getConsortiumName(),
                                               "default", authHelper.getOrganizationId());
        consortium = consortiumService.put(consortium);
        // Need to evict the auth token so we get access to the new consortium
        authHelper.evictToken();
        return new ResponseEntity<>(new ConGetResponse(consortium.getId(),
                                                       consortium.getConsortiumName(),
                                                       consortium.getOrganization()), HttpStatus.OK);
    }

    /**
     * Updates a consortium.
     * @param body request body with consortium name
     * @return the updated consortium
     */
    @RequestMapping(path = "/api/consortiums/{con_id}", method = RequestMethod.PATCH)
    @PreAuthorize("@authHelper.canUpdateConsortium(#consortiumId)")
    public ResponseEntity<ConPatchResponse> updateCon(@PathVariable("con_id") UUID consortiumId,
                                                    @RequestBody ConPatchBody body) {
        Consortium consortium = safeGetConsortium(consortiumId);

        if (body.consortiumName != null) {
            if (body.consortiumName.isBlank()) {
                throw new BadRequestException(ErrorCode.BAD_REQUEST);
            }
            consortium.setConsortiumName(body.getConsortiumName());
            consortium = consortiumService.put(consortium);
        }
        // check if the remove would blow up, and fail before we do anything else
        if (body.getOrgsToRemove() != null && body.getOrgsToRemove().contains(consortium.getOrganization())) {
            // consortium sesvice will look for this, but we might partially process a list
            throw new BadRequestException(ErrorCode.BAD_ORG_REMOVE);
        }

        // we need this for the lambdas
        final Consortium c = consortium;
        if (body.getOrgsToAdd() != null) {
            body.getOrgsToAdd().forEach(o -> consortiumService.addOrganization(c, o));
        }
        if (body.getOrgsToRemove() != null) {
            body.getOrgsToRemove().forEach(o -> consortiumService.removeOrganization(c, o));
        }

        return new ResponseEntity<>(new ConPatchResponse(consortium.getId(),
                                                         consortium.getConsortiumName(),
                                                         consortium.getOrganization(),
                                                         getMembers(consortium)),
                                    HttpStatus.OK);
    }

    /**
     * Return a list of consortium memmbers.
     */
    @RequestMapping(path = "/api/consortiums/{con_id}/organizations")
    @PreAuthorize("@authHelper.canAccessConsortium(#consortiumId)")
    public ResponseEntity<List<OrgGetResponse>> getOrgs(@PathVariable("con_id") UUID consortiumId) {
        Consortium consortium = safeGetConsortium(consortiumId);
        return new ResponseEntity<>(getMembers(consortium),
                                    HttpStatus.OK);
    }

    private List<OrgGetResponse> getMembers(Consortium consortium) {
        return consortiumService.getOrganizations(consortium.getId()).stream()
                .map(o -> new OrgGetResponse(o.getId(), o.getOrganizationName(), o.getOrganizationProperties()))
                .collect(Collectors.toList());
    }

    // This could be done better, but maybe later. We need to handle errors first.
    private Consortium safeGetConsortium(UUID id) {
        Consortium consortium;
        try {
            consortium = consortiumService.get(id);
            if (consortium == null) {
                throw new NotFoundException(ErrorCode.CONSORTIUM_NOT_FOUND, id.toString());
            }
        } catch (NotFoundException e) {
            throw new NotFoundException(ErrorCode.CONSORTIUM_NOT_FOUND, id.toString());
        }

        return consortium;
    }

}
