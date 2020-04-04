/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vmc;

import java.net.URI;

import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import com.google.common.collect.ImmutableSet;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.model.nsx.NsxData;
import com.vmware.blockchain.deployment.services.orchestration.model.vmc.VmcOnAwsData;
import com.vmware.blockchain.deployment.services.restclient.RestClientBuilder;
import com.vmware.blockchain.deployment.services.restclient.RestClientUtils;
import com.vmware.blockchain.deployment.services.restclient.interceptor.LoggingInterceptor;
import com.vmware.blockchain.deployment.services.restclient.interceptor.retry.DefaultHttpRequestRetryInterceptor;

import lombok.Builder;
import lombok.extern.slf4j.Slf4j;

/**
 * An HTTP REST client for issuing API to a VMware Cloud endpoint.
 */
@Slf4j
public class VmcHttpClient {

    private RestTemplate restTemplate;
    private Context context;
    private HttpHeaders httpHeaders;

    /**
     * Constructor.
     */
    public VmcHttpClient(Context context) {
        this.context = context;

        LoggingInterceptor loggingInterceptor = new LoggingInterceptor(
                LoggingInterceptor.ApiLogLevel.URL_STATUS_RESPONSE_FAILURE,
                ImmutableSet.of(VmcEndpoints.VMC_AUTHENTICATION.getPath()),
                ImmutableSet.of("refresh_token", "token", "client_id", "client_secret", "csp-auth-token"),
                ImmutableSet.of());

        CspAuthenticationInterceptor cspAuthenticationInterceptor
                = new CspAuthenticationInterceptor(context.authenticationEndpoint.toString(), context.refreshToken);

        this.restTemplate = new RestClientBuilder().withBaseUrl(context.endpoint.toString())
                .withInterceptor(cspAuthenticationInterceptor)
                .withInterceptor(DefaultHttpRequestRetryInterceptor.getDefaultInstance())
                .withInterceptor(loggingInterceptor)
                .withObjectMapper(RestClientUtils.getDefaultMapper())
                .build();

        httpHeaders = cspAuthenticationInterceptor.getAuthHeaders();
        httpHeaders.setContentType(MediaType.APPLICATION_JSON);
    }

    /**
     * Context parameters for [VmcHttpClient].
     */
    @Builder
    public static class Context {

        URI endpoint;
        URI authenticationEndpoint;
        String refreshToken;
        String organization;
        String datacenter;
    }


    /**
     * Get SDDC information associated with the associated [VmcHttpClient].
     *
     * @return information pertaining to SDDC, as a [Sddc] instance.
     */
    public VmcOnAwsData.Sddc getDataCenterInfo() {
        String uri = VmcEndpoints.VMC_SDDC.getPath().replace("{org}", context.organization)
                .replace("{sddc}", context.datacenter);

        HttpEntity<String> requests = new HttpEntity<>(httpHeaders);
        ResponseEntity<VmcOnAwsData.Sddc> responseEntity
                = restTemplate.exchange(uri, HttpMethod.GET, requests, VmcOnAwsData.Sddc.class);
        if (responseEntity.getStatusCode() != HttpStatus.OK) {
            log.error(responseEntity.toString());
            throw new PersephoneException("Unable to fetch sddc info: " + context.datacenter);
        }
        return responseEntity.getBody();
    }

    /**
     * Allocate a new public IP address for use, with a given name as the identifier.
     *
     * @param name identifier of the IP address resource.
     * @return allocated IP address resource as a instance of [PublicIP], if created, `null` otherwise.
     */
    public NsxData.PublicIp createPublicIp(String name) {
        String uri = resolvePublicIpIdentifier(name);
        HttpEntity<NsxData.PublicIp> requests
                = new HttpEntity<>(NsxData.PublicIp.builder().id(name).displayName(name).build(),
                                   httpHeaders);
        log.info(requests.toString());
        ResponseEntity<NsxData.PublicIp> responseEntity
                = restTemplate.exchange(uri, HttpMethod.PUT, requests, NsxData.PublicIp.class);
        if (responseEntity.getStatusCode() != HttpStatus.OK) {
            throw new PersephoneException("Unable to create public ip: " + name);
        }
        return responseEntity.getBody();
    }

    /**
     * Retrieve the public IP resource with a given resource identifier.
     *
     * @param id identifier of the IP address resource.
     * @return IP address resource as a instance of [PublicIP] if found, `null` otherwise.
     */
    public NsxData.PublicIp getPublicIp(String id) {
        String uri = resolvePublicIpIdentifier(id);
        HttpEntity<NsxData.PublicIp> requests = new HttpEntity<>(httpHeaders);
        ResponseEntity<NsxData.PublicIp> responseEntity
                = restTemplate.exchange(uri, HttpMethod.GET, requests, NsxData.PublicIp.class);
        if (responseEntity.getStatusCode() != HttpStatus.OK) {
            throw new PersephoneException("Error fetching public ip: " + id);
        }
        return responseEntity.getBody();
    }

    /**
     * Create a NAT rule based on the given parameters.
     *
     * @param tier1              tier-1 network that the NAT region is under for NAT rule addition.
     * @param nat                NAT region to add rule to.
     * @param name               identifier to assign to the NAT rule.
     * @param action             type of NAT action.
     * @param sourceNetwork      source address(es) to translate for SNAT and REFLEXIVE rules.
     * @param destinationNetwork destination address(es) to translate for DNAT rules.
     * @param translatedNetwork  network address to apply translation into.
     * @param translatedPorts    network ports to apply translation.
     */
    public NsxData.NatRule createNatRule(String tier1,
                                         String nat,
                                         String name,
                                         NsxData.NatRule.Action action,
                                         String sourceNetwork,
                                         String destinationNetwork,
                                         String translatedNetwork,
                                         String translatedPorts,
                                         int maxRetries,
                                         long maxDelayInSeconds) {
        String uri = VmcEndpoints.NSX_NAT_RULE.getPath()
                .replace("{tier1}", tier1)
                .replace("{nat}", nat)
                .replace("{nat_rule}", name);
        HttpEntity<NsxData.NatRule> requests
                = new HttpEntity<>(NsxData.NatRule.builder().action(action)
                                           .sourceNetwork(sourceNetwork)
                                           .destinationNetwork(destinationNetwork)
                                           .translatedNetwork(translatedNetwork)
                                           .translatedPorts(translatedPorts).build(),
                                   httpHeaders);
        ResponseEntity responseEntity
                = restTemplate.exchange(uri, HttpMethod.PATCH, requests, Void.class);
        if (responseEntity.getStatusCode() != HttpStatus.OK) {
            throw new PersephoneException("Error creating Nat rule: " + name);
        }

        // Check get
        ResponseEntity<NsxData.NatRule> responseEntityGet =
                restTemplate.exchange(uri, HttpMethod.GET, new HttpEntity<>(httpHeaders), NsxData.NatRule.class);

        if (responseEntityGet.getStatusCode() != HttpStatus.OK) {
            throw new PersephoneException("Error getting Nat rule: " + name);
        }
        return responseEntityGet.getBody();
    }

    /**
     * Delete the VMware Cloud resource referenced by the given URI.
     *
     * @param resource resource URI.
     * @return `true` if deleted, `false` otherwise.
     */
    public boolean deleteResource(URI resource) {

        ResponseEntity responseEntity = restTemplate.exchange(resource.toString(), HttpMethod.DELETE,
                                                              new HttpEntity<>(httpHeaders), ResponseEntity.class);
        if (responseEntity.getStatusCode() == HttpStatus.OK) {
            return true;
        }
        if (responseEntity.getStatusCode() == HttpStatus.NOT_FOUND) {
            log.warn("Unable to delete resource {} as its not found", resource);
        }
        return false;
    }

    /**
     * Resolve a public IP identifier to its canonical resource URI as perceived by the target VMware Cloud server
     * endpoint.
     *
     * @param id public IP identifier.
     * @return public IP resource URL as an instance of [URI].
     */
    String resolvePublicIpIdentifier(String id) {
        return context.endpoint + VmcEndpoints.VMC_PUBLIC_IP.getPath().replace("{ip_id}", id);
    }

    /**
     * Resolve a NSX Policy resource identifier to its canonical resource URI as perceived by the target VMware Cloud
     * server endpoint.
     *
     * @param resource NSX policy resource identifier.
     * @return NSX Policy resource URL as an instance of [URI].
     */
    URI resolveNsxResourceIdentifier(String resource) {
        return URI.create(context.endpoint + VmcEndpoints.NSX_API_ROOT.getPath().replace("{resource}", resource));
    }
}