/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import static com.vmware.blockchain.services.blockchains.zones.Zone.Type;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Type.NONE;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Type.ON_PREM;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Type.VMC_AWS;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Type.values;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.common.fleetmanagment.FleetUtils;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.deployment.v1.ListOrchestrationSitesRequest;
import com.vmware.blockchain.deployment.v1.ListOrchestrationSitesResponse;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteServiceStub;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteView;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.OrganizationService;

/**
 * Zone service.  Read the zones from Persephone.
 */
@Service
public class ZoneService {

    private OrchestrationSiteServiceStub client;
    private GenericDao genericDao;
    private OrganizationService organizationService;
    private AuthHelper authHelper;

    @Autowired
    public ZoneService(OrchestrationSiteServiceStub client, GenericDao genericDao,
                       OrganizationService organizationService, AuthHelper authHelper) {
        this.client = client;
        this.genericDao = genericDao;
        this.organizationService = organizationService;
        this.authHelper = authHelper;
    }

    private ImmutableMap<OrchestrationSiteInfo.Type, Type> typeMap
            = ImmutableMap.of(OrchestrationSiteInfo.Type.NONE, NONE,
                              OrchestrationSiteInfo.Type.VMC, VMC_AWS,
                              OrchestrationSiteInfo.Type.VSPHERE, ON_PREM);

    private List<Zone> zones;

    /**
     * (Re)load the zone list.
     */
    public void loadZones()  throws Exception {
        ListOrchestrationSitesRequest request = new ListOrchestrationSitesRequest(new MessageHeader(), 0, "");
        CompletableFuture<ListOrchestrationSitesResponse> future = new CompletableFuture<>();

        client.listOrchestrationSites(request, FleetUtils.blockedResultObserver(future));
        List<OrchestrationSiteView> sites = future.get().getSites();
        zones = sites.stream()
                .map(s -> new Zone(FleetUtils.toUuid(s.getId()), typeMap.get(s.getType()), s.getLabels()))
                .collect(
                        Collectors.toList());
    }

    /**
     * Get all zones.  Currently that means we also add in the list of zones supplied by Persephone.
     * @return all zones.
     */
    public List<Zone> getZones() {
        // make a copy of the zones list
        List<Zone> allZones = new ArrayList<>(zones);
        // add in everything in the DB
        allZones.addAll(genericDao.getAllByType(Zone.class));
        return allZones;
    }

    public Zone put(Zone zone) {
        return genericDao.put(zone, null);
    }

    /**
     * Get a specific zone.  If not in database, look in default list.  Throw NotFound if no match.
     */
    public Zone get(UUID id) {
        try {
            return genericDao.get(id, Zone.class);
        } catch (NotFoundException e) {
            // Check the zones list
            Optional<Zone> oz = zones.stream().filter(z -> z.getId().equals(id)).findFirst();
            if (oz.isEmpty()) {
                throw e;
            }
            return oz.get();
        }
    }

    public List<Zone> getByType(Zone.Type type) {
        String json = JSONObject.toJSONString(Collections.singletonMap("type", type.toString()));
        return genericDao.getByJsonQuery(json, Zone.class);
    }

    public List<OnpremZone> getOnpremZones(UUID orgId) {
        String json = JSONObject.toJSONString(Collections.singletonMap("type", ON_PREM.toString()));
        return genericDao.getJsonByParentQuery(orgId, json, OnpremZone.class);
    }

    /**
     * Get a zone from a oranazation and type.
     */
    public List<Zone> getByOrgAndType(UUID orgId, Type type) {
        // Get the Persephone zones of this type
        List<Zone> allZones = new ArrayList<>(zones.stream().filter(zone -> zone.getType().equals(type))
                                                      .collect(Collectors.toList()));
        String json = JSONObject.toJSONString(Collections.singletonMap("type", type.toString()));
        // Add in anything in the DB.
        allZones.addAll(genericDao.getJsonByParentQuery(orgId, json, Zone.class));
        return allZones;
    }

    /**
     * Get the list of zones this organization has access to.
     *  Check to see of the ORG_ZONES property has been set.
     *  If not, select all zone types
     *  If so, select the zone types set in the properties
     *  Collect all the zone this organization can see.
     */
    public List<Zone> getByOrganization(Organization organization) {
        // zoneTypes will contain the types of zones we can see.
        List<Type> zoneTypes = getAuthorizedTypes(organization);
        List<Zone> zones = zoneTypes.stream()
                .map(t -> getByOrgAndType(organization.getId(), t))
                .flatMap(z -> z.stream())
                .collect(Collectors.toList());
        return zones;
    }

    private List<Type> getAuthorizedTypes(Organization organization) {
        List<Type> zoneTypes;
        // See if the org properties has a list of valid zones.
        Map<String, String> properties = organization.getOrganizationProperties();
        String zoneString = properties == null ? "" : properties.getOrDefault(Constants.ORG_ZONES, "");
        if (zoneString.isBlank()) {
            zoneTypes = Arrays.stream(values()).collect(Collectors.toList());
        } else {
            // gets the zones, and removes any white space
            String[] zoneStrings = zoneString.strip().split("\\s*,\\s*");
            zoneTypes = Arrays.stream(zoneStrings).map(Type::valueOf).collect(Collectors.toList());
        }
        return zoneTypes;
    }

    /**
     * Get all the Zones this user is authorized to see.
     * @return List of zones
     */
    public List<Zone> getAllAuthorized() {
        if (authHelper.isSystemAdmin()) {
            return getZones();
        } else {
            Organization org = organizationService.get(authHelper.getOrganizationId());
            return getByOrganization(org);
        }

    }

    /**
     * Get the specific zone if this user is authorized to see it.
     * @param zoneId Zone we want
     * @return Zone if we are authorized to see it.
     */
    public Zone getAuthorized(UUID zoneId) {
        Zone zone = get(zoneId);
        if (authHelper.isSystemAdmin()) {
            return zone;
        }
        // Fist check if we have access to this type of zone
        Organization org = organizationService.get(authHelper.getOrganizationId());
        if (getAuthorizedTypes(org).contains(zone.getType())) {
            // next, if this is an onprem zone, see if this belongs to our org
            if (zone.getType().equals(ON_PREM)) {
                if (!((OnpremZone) zone).getOrgId().equals(org.getId())) {
                    throw new NotFoundException(ErrorCode.NOT_FOUND);
                }
            }
            return zone;
        }
        throw new NotFoundException(ErrorCode.NOT_FOUND);
    }

    public void delete(UUID zoneId) {
        genericDao.delete(zoneId, Zone.class);
    }
}
