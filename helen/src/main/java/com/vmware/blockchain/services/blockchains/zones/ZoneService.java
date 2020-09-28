/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import static com.vmware.blockchain.services.blockchains.zones.Zone.Type;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Type.NONE;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Type.ON_PREM;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Type.VMC_AWS;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Type.values;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.google.common.base.Strings;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.OrganizationService;

import lombok.extern.slf4j.Slf4j;


/**
 * Zone service.  Read the zones from Persephone.
 */
@Service
@Slf4j
public class ZoneService {

    private GenericDao genericDao;
    private OrganizationService organizationService;
    private AuthHelper authHelper;
    private Set<String> defaultZoneIdSet;
    private String defaultZoneIdListString;

    @Autowired
    public ZoneService(GenericDao genericDao,
                       OrganizationService organizationService, AuthHelper authHelper,
                       @Value("${vmbc.enabled.vmc.zones:#{null}}") String defaultZoneIdListString) {
        this.genericDao = genericDao;
        this.organizationService = organizationService;
        this.authHelper = authHelper;
        this.defaultZoneIdListString = defaultZoneIdListString;

        defaultZoneIdSet = new HashSet<>();
        if (!Strings.isNullOrEmpty(defaultZoneIdListString)) {
            Collections.addAll(defaultZoneIdSet, defaultZoneIdListString.split(","));
        }
    }

    /**
     * Get a specific zone.  If not in database, look in default list.  Throw NotFound if no match.
     * @param id zoneId for lookup
     * @return Zone.class
     */
    public Zone get(UUID id) {
        return genericDao.get(id, Zone.class);
    }

    /**
     * Create a new zone.
     * @param zone zone to be created
     * @return Zone.class
     */
    public Zone put(Zone zone) {
        return genericDao.put(zone, null);
    }

    /**
     * Delete a zone.
     * @param zoneId Zone to be deleted
     */
    public void delete(UUID zoneId) {
        genericDao.delete(zoneId, Zone.class);
    }

    /**
     * Get default zones based on the configured Spring property.
     * Typically used for VMC_AWS zones, but could be either VMC_AWS or ON_PREM.
     * This method will load default zones strictly based on the configured Spring property.
     * Its caller methods can decide additional zones to add to the results of the API call.
     * @return List of Zone.class
     */
    public List<Zone> getDefaultZones() {
        List<Zone> allZones = genericDao.getAllByType(Zone.class);
        List<Zone> defaultZones = defaultZoneIdSet.size() > 0
                ? allZones.stream().filter(zone -> defaultZoneIdSet.contains(zone.getId().toString()))
                                   .collect(Collectors.toList())
                : allZones;
        log.info("List of default zones: {}", defaultZones.toString());
        log.info("Default zoneIds: {}", defaultZoneIdListString);
        return defaultZones;
    }

    /**
     * Get the specific zone if this user is authorized to see it.
     * @param zoneId Zone we want
     * @return Zone if we are authorized to see it.
     */
    Zone getAuthorized(UUID zoneId) {
        Zone zone = get(zoneId);
        if (authHelper.isSystemAdmin()) {
            return zone;
        }
        // Fist check if we have access to this type of zone
        Organization org = organizationService.get(authHelper.getOrganizationId());
        if (getAuthorizedTypes(org).contains(zone.getType())) {
            // next, if this is an ON_PREM zone, see if this belongs to our org
            // BC-4644:If this zone was created by an admin account, the org in the zone is null
            // TODO: Need to nail down the real use case for this.
            if (zone.getType().equals(ON_PREM)) {
                if (!org.getId().equals(zone.getOrgId())) {
                    throw new NotFoundException(ErrorCode.NOT_FOUND);
                }
            }
            return zone;
        }
        throw new NotFoundException(ErrorCode.NOT_FOUND);
    }

    /**
     * Get all the Zones this user is authorized to see.
     * @return List of Zone.class
     */
    List<Zone> getAllAuthorized() {
        if (authHelper.isSystemAdmin()) {
            return getZones();
        } else {
            Organization org = organizationService.get(authHelper.getOrganizationId());
            return getByOrganization(org);
        }

    }

    /**
     * Get all zones.  Currently that means we add any OnPrem zones created, in addition to default zones.
     * @return List of Zone.class
     */
    List<Zone> getZones() {
        // Get all default zones whether VMC_AWS or ON_PREM
        List<Zone> allZones = getDefaultZones();

        // VMC_AWS zones are strictly only loaded from getDefaultZones
        // Add all additional ON_PREM/NONE zones from the DB, which aren't present in default zones
        List<Zone> additionalZones = getAllZonesByType(ON_PREM);
        additionalZones.addAll(getAllZonesByType(NONE));
        allZones.addAll(additionalZones.stream()
                            .filter(zone -> zone.getOrgId() != null)
                            .filter(zone -> !defaultZoneIdSet.contains(zone.getId().toString()))
                            .collect(Collectors.toList()));
        return allZones;
    }

    /**
     * Get the list of zones this organization has access to.
     * Check to see of the ORG_ZONES property has been set.
     * If not, select all zone types
     * If so, select the zone types set in the properties
     * Collect all the zone this organization can see.
     */
    List<Zone> getByOrganization(Organization organization) {
        // zoneTypes will contain the types of zones we can see.
        List<Type> zoneTypes = getAuthorizedTypes(organization);
        List<Zone> zones = zoneTypes.stream()
                .map(type -> getByOrgAndType(organization.getId(), type))
                .flatMap(Collection::stream)
                .collect(Collectors.toList());
        return zones;
    }

    List<Type> getAuthorizedTypes(Organization organization) {
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
     * Get a zone by organization and type, applying filters on default zones.
     */
    List<Zone> getByOrgAndType(UUID orgId, Type type) {
        // Get the default zones of this org and type
        List<Zone> defaultOrgAndTypeZones = getDefaultZones().stream()
                .filter(zone -> zone.getType().equals(type))
                // TODO: Default zones don't have an orgId. Hermes fails if filtered on orgId for default zones.
                // .filter(zone -> zone.getOrgId() != null && zone.getOrgId().toString().equals(orgId.toString()))
                .collect(Collectors.toList());
        if (type == VMC_AWS) {
            // If type is VMC_AWS, only return filtered default zones
            return defaultOrgAndTypeZones;
        } else {
            // For type ON_PREM or NONE, add additional zones of this org and type that are not in default zones
            // Filtering out default zones here in case there are default ON_PREM/NONE zones in migrations
            String json = JSONObject.toJSONString(Collections.singletonMap("type", type.toString()));
            List<Zone> additionalOrgAndTypeZones = genericDao.getJsonByParentQuery(orgId, json, Zone.class).stream()
                                                    .filter(zone -> !defaultZoneIdSet.contains(zone.getId().toString()))
                                                    .collect(Collectors.toList());
            additionalOrgAndTypeZones.addAll(defaultOrgAndTypeZones);
            return additionalOrgAndTypeZones;
        }
    }

    List<Zone> getAllZonesByType(Zone.Type type) {
        String json = JSONObject.toJSONString(Collections.singletonMap("type", type.toString()));
        return genericDao.getByJsonQuery(json, Zone.class);
    }

    List<OnPremZone> getOnpremZones(UUID orgId) {
        String json = JSONObject.toJSONString(Collections.singletonMap("type", ON_PREM.toString()));
        return genericDao.getJsonByParentQuery(orgId, json, OnPremZone.class);
    }

}
