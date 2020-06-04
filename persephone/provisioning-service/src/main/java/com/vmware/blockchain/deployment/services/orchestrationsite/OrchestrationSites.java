/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.services.orchestrationsite;

import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.util.Strings;

import com.google.protobuf.InvalidProtocolBufferException;
import com.google.protobuf.util.JsonFormat;
import com.vmware.blockchain.deployment.v1.ElasticSearch;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.LogManagement;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo;
import com.vmware.blockchain.deployment.v1.VSphereOrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.VmcOrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.Wavefront;

import lombok.extern.slf4j.Slf4j;

/**
 * Helper class for @OrchestrationSite.
 */

@Slf4j
public class OrchestrationSites {

    /**
     * Helper util to override build site.
     * @param originalSiteInfo info
     * @param containerRegistry registry
     * @return info
     */
    public static final OrchestrationSiteInfo buildSiteInfo(OrchestrationSiteInfo originalSiteInfo,
                                                            Endpoint containerRegistry) {
        OrchestrationSiteInfo original = originalSiteInfo;
        if (originalSiteInfo.getType() == OrchestrationSiteInfo.Type.VSPHERE) {
            if (Strings.isEmpty(originalSiteInfo.getVsphere().getContainerRegistry().getAddress())) {
                original = OrchestrationSiteInfo.newBuilder(original)
                        .setVsphere(VSphereOrchestrationSiteInfo.newBuilder(original.getVsphere())
                                            .setContainerRegistry(containerRegistry).build()).build();
            }
        }

        if (original.getType() == OrchestrationSiteInfo.Type.VMC) {
            if (Strings.isEmpty(originalSiteInfo.getVmc().getContainerRegistry().getAddress())) {

                original = OrchestrationSiteInfo.newBuilder(original)
                        .setVmc(VmcOrchestrationSiteInfo.newBuilder(original.getVmc())
                                        .setContainerRegistry(containerRegistry).build()).build();
            }
        }

        return original;
    }

    /**
     * Get Logging info.
     * @param siteInfo OrchestrationSiteInfo
     * @return info
     */
    public static String getLogManagementJson(OrchestrationSiteInfo siteInfo) {

        List<LogManagement> logManagements = new ArrayList<>();

        switch (siteInfo.getType()) {
            case VMC:
                logManagements.addAll(siteInfo.getVmc().getLogManagementsList());
                break;
            case VSPHERE:
                logManagements.addAll(siteInfo.getVsphere().getLogManagementsList());
                break;
            default:
                break;
        }

        // TODO: It takes only the first one, subject to change if design changes.
        if (!logManagements.isEmpty()) {
            LogManagement logManagement = logManagements.get(0);
            try {
                return JsonFormat.printer().print(logManagement);
            } catch (InvalidProtocolBufferException e) {
                log.error("error parsing log info" + e);
            }
        }
        return "";
    }

    /**
     * Gets wavefront details.
     * @param siteInfo OrchestrationSiteInfo
     * @return Wavefront
     */
    public static Wavefront getWavefront(OrchestrationSiteInfo siteInfo) {
        Wavefront wavefront = Wavefront.newBuilder().build();
        switch (siteInfo.getType()) {
            case VMC:
                wavefront = siteInfo.getVmc().getWavefront();
                break;
            case VSPHERE:
                wavefront = siteInfo.getVsphere().getWavefront();
                break;
            default:
                break;
        }
        return wavefront;
    }

    /**
     * Get elasticsearch details.
     * @param siteInfo OrchestrationSiteInfo
     * @return ElasticSearch
     */
    public static ElasticSearch getElasticsearch(OrchestrationSiteInfo siteInfo) {
        ElasticSearch elasticSearch = ElasticSearch.newBuilder().build();
        switch (siteInfo.getType()) {
            case VMC:
                elasticSearch = siteInfo.getVmc().getElasticsearch();
                break;
            case VSPHERE:
                elasticSearch = siteInfo.getVsphere().getElasticsearch();
                break;
            default:
                break;
        }
        return elasticSearch;
    }

    /**
     * Gets the outbound proxy information for on-prem deployment when present.
     * @param siteInfo OrchestrationSiteInfo
     * @return OutboundProxyInfo
     */
    public static OutboundProxyInfo getOutboundProxy(OrchestrationSiteInfo siteInfo) {
        OutboundProxyInfo outboundProxyInfo = OutboundProxyInfo.newBuilder().build();
        switch (siteInfo.getType()) {
            case VMC:
                outboundProxyInfo = siteInfo.getVmc().getVsphere().getOutboundProxy();
                break;
            case VSPHERE:
                outboundProxyInfo = siteInfo.getVsphere().getVsphere().getOutboundProxy();
                break;
            default:
                break;
        }
        return outboundProxyInfo;
    }

}