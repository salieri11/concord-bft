/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.lang.String;
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
 * Controller to handle Consortium.
 */
@RestController
public class ConsortiumController {

    public ConsortiumService consortiumService;

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
       // String consortiumType;
    }

    @Autowired
    public ConsortiumController(ConsortiumService consortiumService) {
        this.consortiumService = consortiumService;
    }

    /**
     * List all consortium.
     * @return
     */
    @RequestMapping(path = "/api/consortiums", method = RequestMethod.GET)
    public ResponseEntity<List<ConGetResponse>> lisCons() {
        List<Consortium> orgs = consortiumService.list();
        List<ConGetResponse> rList = orgs.stream().map(c -> new ConGetResponse(c.getId(), c.getConsortiumName()))
                .collect(Collectors.toList());
        return new ResponseEntity<>(rList, HttpStatus.OK);
    }

    /**
     * List all consortiums.
     * @return
     */
    @RequestMapping(path = "/api/consortiums/{con_id}", method = RequestMethod.GET)
    public ResponseEntity<ConGetResponse> getCon(@PathVariable("con_id") UUID consortiumId) {
        Consortium consortium = consortiumService.get(consortiumId);
        return new ResponseEntity<>(new ConGetResponse(consortium.getId(), consortium.getConsortiumName()), HttpStatus.OK);
    }

    /**
     * Creates a new consortium.
     * @param body request body with consortium name
     * @return the new consortium
     */
    @RequestMapping(path = "/api/consortiums", method = RequestMethod.POST)
    public ResponseEntity<ConGetResponse> createCOn(@RequestBody ConPostBody body) {
        Consortium consortium = new Consortium(body.getConsortiumName());
        consortium = consortiumService.put(consortium);
        return new ResponseEntity<>(new ConGetResponse(consortium.getId(), consortium.getConsortiumName()), HttpStatus.OK);
    }

    /**
     * Updates an organization.
     * @param body request body with consortium name
     * @return the new consortium
     */
    @RequestMapping(path = "/api/consortiums/{con_id}", method = RequestMethod.PATCH)
    public ResponseEntity<ConGetResponse> updateCon(@PathVariable("con_id") UUID consortiumId, @RequestBody ConPostBody body) {
        Consortium consortium = consortiumService.get(consortiumId);
        if (body.consortiumName != null) {
            consortium.setConsortiumName(body.getConsortiumName());
        }
        consortium = consortiumService.put(consortium);
        return new ResponseEntity<>(new ConGetResponse(consortium.getId(), consortium.getConsortiumName()), HttpStatus.OK);
    }
}
