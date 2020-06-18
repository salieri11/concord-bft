/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.orchestration.vsphere;

import java.net.URI;
import java.util.Arrays;
import java.util.Base64;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.assertj.core.util.Strings;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableSet;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.GetDatastoreResponse;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.GetFolderResponse;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.GetNetworkResponse;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.GetResourcePoolResponse;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.LibraryItemDeploy;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.LibraryItemFindRequest;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.LibraryItemFindResponse;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.LibraryItemFindSpec;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.NetworkMapping;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.OvfParameter;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.OvfProperty;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.VirtualMachinePowerResponse;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.VirtualMachinePowerState;
import com.vmware.blockchain.deployment.services.orchestration.model.vsphere.VirtualMachineUpdate;
import com.vmware.blockchain.deployment.services.orchestration.vm.CloudInitConfiguration;
import com.vmware.blockchain.deployment.services.restclient.RestClientBuilder;
import com.vmware.blockchain.deployment.services.restclient.RestClientUtils;
import com.vmware.blockchain.deployment.services.restclient.interceptor.LoggingInterceptor;
import com.vmware.blockchain.deployment.services.restclient.interceptor.retry.DefaultHttpRequestRetryInterceptor;
import com.vmware.blockchain.deployment.v1.NodeProperty;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import lombok.val;

/**
 * An HTTP REST client for issuing API to a vSphere endpoint.
 */
@Slf4j
public class VSphereHttpClient {
    private RestTemplate restTemplate;
    private Context context;
    private HttpHeaders httpHeaders;

    private VsphereSessionAuthenticationInterceptor vsphereSessionAuthenticationInterceptor;
    private LoggingInterceptor loggingInterceptor;

    /**
     * Constructor.
     */
    public VSphereHttpClient(Context context) {
        this.context = context;

        loggingInterceptor = new LoggingInterceptor(
                LoggingInterceptor.ApiLogLevel.URL_STATUS_RESPONSE_FAILURE,
                ImmutableSet.of(),
                ImmutableSet.of(),
                ImmutableSet.of());

        vsphereSessionAuthenticationInterceptor
                = new VsphereSessionAuthenticationInterceptor(context.getEndpoint().toString(), context.getUsername(),
                context.getPassword());

        this.restTemplate = restTemplate();

        httpHeaders = vsphereSessionAuthenticationInterceptor.getAuthHeaders();
        httpHeaders.setContentType(MediaType.APPLICATION_JSON);
    }

    /**
     * Get rest template built with the available information.
     *
     * @return Built RestTemplate.
     */
    public RestTemplate restTemplate() {
        return new RestClientBuilder().withBaseUrl(context.getEndpoint().toString())
                .withInterceptor(vsphereSessionAuthenticationInterceptor)
                .withInterceptor(DefaultHttpRequestRetryInterceptor.getDefaultInstance())
                .withInterceptor(loggingInterceptor)
                .withObjectMapper(RestClientUtils.getDefaultMapper())
                .build();
    }

    /**
     * Context parameters for [VSphereHttpClient].
     */
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    @Getter
    public static class Context {
        URI endpoint;
        String username;
        String password;

    }


    /**
     * Get ID of a specified folder based on name and type.
     *
     * @param name name of the folder to look up.
     * @return ID of the folder as a [String], if found.
     */
    public String getFolder(String name) {
        String type = "VIRTUAL_MACHINE";
        String uri = VsphereEndpoints.VSPHERE_FOLDERS.getPath() + "?filter.type=" + type + "&filter.names=" + name;

        HttpEntity<String> requests = new HttpEntity<>(httpHeaders);
        ResponseEntity<GetFolderResponse> responseEntity
                = restTemplate.exchange(uri, HttpMethod.GET, requests, GetFolderResponse.class);

        if (responseEntity.getStatusCode() == HttpStatus.OK
            && responseEntity.getBody().getValue().size() > 0
            && responseEntity.getBody().getValue().get(0) != null
            && !Strings.isNullOrEmpty(responseEntity.getBody().getValue().get(0).getFolder())) {
            return responseEntity.getBody().getValue().get(0).getFolder();
        }

        throw new PersephoneException("Error retrieving folder: " + name);
    }

    /**
     * Get ID of a specified resource pool based on name.
     *
     * @return ID of the resource pool as a [String], if found.
     */
    public String getResourcePool(String name) {
        String uri = VsphereEndpoints.VSPHERE_RESOURCE_POOLS.getPath() + "?filter.names=" + name;

        HttpEntity<String> requests = new HttpEntity<>(httpHeaders);
        ResponseEntity<GetResourcePoolResponse> responseEntity
                = restTemplate.exchange(uri, HttpMethod.GET, requests, GetResourcePoolResponse.class);

        if (responseEntity.getStatusCode() == HttpStatus.OK
            && responseEntity.getBody().getValue().size() > 0
            && responseEntity.getBody().getValue().get(0) != null
            && !Strings.isNullOrEmpty(responseEntity.getBody().getValue().get(0).getResourcePool())) {
            return responseEntity.getBody().getValue().get(0).getResourcePool();
        }
        throw new PersephoneException("Error retrieving resource pool: " + name);
    }

    /**
     * Get ID of a specified datastore based on name.
     *
     * @param name name of the datastore to look up.
     * @return ID of the datastore as a [String], if found.
     */
    public String getDatastore(String name) {
        String uri = VsphereEndpoints.VSPHERE_DATASTORES.getPath() + "?filter.names=" + name;
        HttpEntity<String> requests = new HttpEntity<>(httpHeaders);
        ResponseEntity<GetDatastoreResponse> responseEntity
                = restTemplate.exchange(uri, HttpMethod.GET, requests, GetDatastoreResponse.class);

        if (responseEntity.getStatusCode() == HttpStatus.OK
            && responseEntity.getBody().getValue().size() > 0
            && responseEntity.getBody().getValue().get(0) != null
            && !Strings.isNullOrEmpty(responseEntity.getBody().getValue().get(0).getDatastore())) {
            return responseEntity.getBody().getValue().get(0).getDatastore();
        }
        throw new PersephoneException("Error retrieving datastore: " + name);
    }

    /**
     * Get ID of a specified logical network port-group based on name.
     *
     * @param name name of the network to look up.
     * @return ID of the network as a [String], if found.
     */
    public String getNetwork(String name) {
        String uri = VsphereEndpoints.VSPHERE_NETWORKS.getPath() + "?filter.names=" + name;

        HttpEntity<String> requests = new HttpEntity<>(httpHeaders);
        ResponseEntity<GetNetworkResponse> responseEntity
                = restTemplate.exchange(uri, HttpMethod.GET, requests, GetNetworkResponse.class);

        if (responseEntity.getStatusCode() == HttpStatus.OK
            && responseEntity.getBody().getValue().size() > 0
            && responseEntity.getBody().getValue().get(0) != null
            && !Strings.isNullOrEmpty(responseEntity.getBody().getValue().get(0).getNetwork())) {
            return responseEntity.getBody().getValue().get(0).getNetwork();
        }
        throw new PersephoneException("Error retrieving network: " + name);
    }

    /**
     * Get ID of the specified content library item based on name.
     *
     * @param sourceId unique ID of the library item as declared by publishing source.
     * @return ID of the library item as a [String], if found.
     */
    public String getLibraryItem(String sourceId) {
        String uri = VsphereEndpoints.VSPHERE_CONTENT_LIBRARY_ITEM.getPath() + "?~action=find";
        HttpEntity<LibraryItemFindRequest> requests = new HttpEntity<>(LibraryItemFindRequest.builder()
                                                                               .spec(LibraryItemFindSpec.builder()
                                                                                             .sourceId(sourceId)
                                                                                             .build())
                                                                               .build(),
                                                                       httpHeaders);
        ResponseEntity<LibraryItemFindResponse> responseEntity
                = restTemplate.exchange(uri, HttpMethod.POST, requests, LibraryItemFindResponse.class);

        if (responseEntity.getStatusCode() == HttpStatus.OK
            && responseEntity.getBody().getValue().size() > 0
            && !Strings.isNullOrEmpty(responseEntity.getBody().getValue().get(0))) {
            return responseEntity.getBody().getValue().get(0);
        }
        throw new PersephoneException("Error retrieving library item: " + sourceId);
    }

    /**
     * Create a housing instance based on the specified parameters.
     */
    public String createVirtualMachine(String name,
                                       String libraryItem,
                                       String datastore,
                                       String resourcePool,
                                       String folder,
                                       Map.Entry<String, String> networks,
                                       CloudInitConfiguration cloudInit) {

        val encodedUserData = Base64.getEncoder().encodeToString(cloudInit.userData().getBytes());

        val deployRequest = LibraryItemDeploy.LibraryItemDeployRequest.builder()
                .deploymentSpec(
                        LibraryItemDeploy.LibraryItemDeploymentSpec.builder()
                                .name(name)
                                .eula(true)
                                .defaultDatastoreId(datastore)
                                .networkMappings(
                                        Arrays.asList(new NetworkMapping(networks.getKey(), networks.getValue())))
                                .additionalParameters(
                                        Arrays.asList(
                                                OvfParameter.builder().atClass(
                                                        OvfParameter.OvfParameterTypes.PROPERTY_PARAMS.classValue)
                                                        .type(OvfParameter.OvfParameterTypes.PROPERTY_PARAMS.type)
                                                        .properties(new OvfProperty[]{
                                                                OvfProperty.builder().id("instance-id").value(name)
                                                                        .build(),
                                                                OvfProperty.builder().id("hostname").value(name)
                                                                        .build(),
                                                                OvfProperty.builder().id("user-data")
                                                                        .value(encodedUserData).build()}).build()))
                                .build())
                .target(LibraryItemDeploy.LibraryItemDeploymentTarget.builder()
                                .resourcePoolId(resourcePool)
                                .folderId(folder).build()).build();

        String uri = VsphereEndpoints.VSPHERE_OVF_LIBRARY_ITEM.getPath().replace("{library_item}", libraryItem)
                     + "?~action=deploy";

        HttpEntity<LibraryItemDeploy.LibraryItemDeployRequest> requests =
                new HttpEntity<>(deployRequest, httpHeaders);

        // TODO Remove after stability.
        try {
            ObjectMapper mapper = new ObjectMapper();
            log.info("******************* CreateVirtualMachine request********************");
            log.info(mapper.writerWithDefaultPrettyPrinter().writeValueAsString(requests));
        } catch (Exception e) {
            log.warn(e.toString());
        }

        ResponseEntity<LibraryItemDeploy.LibraryItemDeployResponse> responseEntity
                = restTemplate
                .exchange(uri, HttpMethod.POST, requests, LibraryItemDeploy.LibraryItemDeployResponse.class);

        if (responseEntity.getStatusCode() == HttpStatus.OK) {
            return responseEntity.getBody().getValue().getResourceId().getId();
        }

        throw new PersephoneException("Error creating VM", name);
    }

    private boolean updateVmHardware(String name, Map<String, String> properties) {

        boolean mem = true;
        boolean cpu = true;
        if (properties.containsKey(NodeProperty.Name.VM_MEMORY.toString())) {
            long memoryMb = Long.parseLong(properties
                    .getOrDefault(NodeProperty.Name.VM_MEMORY.toString(), "16"))  * 1024;
            mem = updateVirtualMachineMemory(name, memoryMb);
        }

        if (properties.containsKey(NodeProperty.Name.VM_CPU_COUNT.toString())) {
            int cpuCount = Integer.parseInt(properties
                    .getOrDefault(NodeProperty.Name.VM_CPU_COUNT.toString(), "2"));
            int coresPerSocket = Integer.parseInt(properties
                    .getOrDefault(NodeProperty.Name.VM_CORES_PER_SOCKET.toString(), "1"));
            cpu = updateVirtualMachineCpu(name, cpuCount, coresPerSocket);
        }
        return mem && cpu;
    }

    /**
     * Update virtual machine memory.
     */
    public boolean updateVirtualMachineMemory(String name, long memory) {
        val updateRequest = VirtualMachineUpdate.VirtualMachineUpdateRequest.builder()
                .spec(
                        VirtualMachineUpdate.VirtualMachineUpdateSpec.builder()
                                .hotAddEnabled(true)
                                .sizeMiB(memory).build())
                .build();
        HttpEntity<VirtualMachineUpdate.VirtualMachineUpdateRequest> requests =
                new HttpEntity<>(updateRequest, httpHeaders);
        String uri = VsphereEndpoints.VSPHERE_VM_MEMORY_UPDATE.getPath().replace("{vm}", name);
        try {
            log.info("******************* updateVirtualMachineMemory request********************");
            ObjectMapper mapper = new ObjectMapper();
            log.info(mapper.writerWithDefaultPrettyPrinter().writeValueAsString(requests));
        } catch (Exception e) {
            log.warn(e.toString());
        }

        var responseEntity = restTemplate.exchange(uri, HttpMethod.PATCH, requests, Void.class);
        return responseEntity.getStatusCode() == HttpStatus.OK;
    }

    /**
     * Update virtual machine vcpu and cores per cpu.
     */
    public boolean updateVirtualMachineCpu(String name, int cpuCount, int coresPerSocket) {
        val updateRequest = VirtualMachineUpdate.VirtualMachineUpdateRequest.builder()
                .spec(
                        VirtualMachineUpdate.VirtualMachineUpdateSpec.builder()
                                .count(cpuCount)
                                .coresPerSocket(coresPerSocket)
                                .hotAddEnabled(true)
                                .hotRemoveEnabled(true)
                                .build())
                .build();
        HttpEntity<VirtualMachineUpdate.VirtualMachineUpdateRequest> requests =
                new HttpEntity<>(updateRequest, httpHeaders);
        String uri = VsphereEndpoints.VSPHERE_VM_CPU_UPDATE.getPath().replace("{vm}", name);
        try {
            ObjectMapper mapper = new ObjectMapper();
            log.info("******************* updateVirtualMachineCpu request********************");
            log.info(mapper.writerWithDefaultPrettyPrinter().writeValueAsString(requests));
        } catch (Exception e) {
            log.warn(e.toString());
        }

        var responseEntity = restTemplate.exchange(uri, HttpMethod.PATCH, requests, Void.class);
        return responseEntity.getStatusCode() == HttpStatus.OK;
    }

    /**
     * Delete the virtual machine referenced by the given identifier.
     *
     * @param id identifier of the virtual machine.
     * @return `true` if deleted, `false` otherwise.
     */
    public boolean deleteVirtualMachine(String id) throws HttpClientErrorException {
        String uri = VsphereEndpoints.VSPHERE_VM.getPath().replace("{vm}", id);
        HttpEntity requests = new HttpEntity<>(httpHeaders);
        try {
            ResponseEntity entity = restTemplate.exchange(uri, HttpMethod.DELETE, requests, Void.class);
            return true;
        } catch (HttpClientErrorException e) {
            // Expected behavior
            // true is only returned for (404) Not Found VMs
            // All other exceptions are false
            return e.getRawStatusCode() == 404;
        }
    }

    /**
     * Update the power state of a virtual machine specified by the given parameter.
     *
     * @param name  identifier of the virtual machine.
     * @param state power state to update to.
     * @return `true` if power operation is successfully executed for the specified virtual machine, `false` otherwise.
     */
    boolean updateVirtualMachinePowerState(String name, VirtualMachinePowerState state) {
        String uri = null;

        if (VirtualMachinePowerState.POWERED_OFF == state) {
            uri = VsphereEndpoints.VSPHERE_VM_POWER_STOP.getPath();
        } else if (VirtualMachinePowerState.POWERED_ON == state) {
            uri = VsphereEndpoints.VSPHERE_VM_POWER_START.getPath();
        }

        uri = uri.replace("{vm}", name);
        HttpEntity<String> requests = new HttpEntity<>(httpHeaders);
        ResponseEntity responseEntity
                = restTemplate.exchange(uri, HttpMethod.POST, requests, Void.class);

        return responseEntity.getStatusCode() == HttpStatus.OK;
    }

    /**
     * Get the power state of a virtual machine.
     *
     * @param name identifier of the virtual machine.
     * @return the power state of the virtual machine expressed as [VirtualMachinePowerState], `null` if the operation
     * cannot be executed or if the virtual machine does not exist.
     */
    VirtualMachinePowerState getVirtualMachinePower(String name) throws HttpClientErrorException {
        log.info("Get VM Power state details.");
        String uri = VsphereEndpoints.VSPHERE_VM_POWER.getPath().replace("{vm}", name);

        HttpEntity requests = new HttpEntity<>(httpHeaders);
        try {
            ResponseEntity<VirtualMachinePowerResponse> responseEntity
                    = restTemplate.exchange(uri, HttpMethod.GET, requests, VirtualMachinePowerResponse.class);

            return responseEntity.getBody().getValue().getState();
        } catch (HttpClientErrorException e) {
            throw new PersephoneException("Unable to get VM info: " + name);
        }
    }

    /**
     * Ensure the power-on state of the virtual machine specified by the given parameter.
     *
     * @param name          identifier of the virtual machine.
     * @param retryInterval interval to retry checking for guest OS's power state, in milliseconds.
     * @return `true` if the power state of the virtual machine is on at some point during the execution of the
     * function, `false` otherwise.
     */
    public boolean ensureVirtualMachinePowerStart(String name, Long retryInterval, Map<String, String> properties) {
        var confirmed = false;
        var iterating = true;
        while (iterating) {
            // The typical case.
            log.info("Iterating.");
            if (!updateVmHardware(name, properties)) {
                log.error("Updating VM hardware failed.");
                return false;
            }
            updateVirtualMachinePowerState(name, VirtualMachinePowerState.POWERED_ON);
            VirtualMachinePowerState state = getVirtualMachinePower(name);
            if (state == VirtualMachinePowerState.POWERED_OFF || state == VirtualMachinePowerState.SUSPEND) {
                updateVirtualMachinePowerState(name, VirtualMachinePowerState.POWERED_ON);

                // Do not engage next iteration immediately.
                try {
                    Thread.sleep(retryInterval);
                } catch (Exception e) {
                    ///
                }

            } else if (state == VirtualMachinePowerState.POWERED_ON) {
                iterating = false;
                confirmed = true;
            } else {
                // If the VM doesn't exist or power-state cannot be retrieved.
                iterating = false;
                confirmed = false;
            }
        }
        return confirmed;
    }

    /**
     * Ensure the power-off state of the virtual machine specified by the given parameter.
     *
     * @param name identifier of the virtual machine.
     * @return `true` if the power state of the virtual machine is off at some point during the execution of the
     * function, `false` otherwise.
     */
    public boolean ensureVirtualMachinePowerStop(String name) {
        var confirmed = false;
        var iterating = true;
        var attempts = 0;
        updateVirtualMachinePowerState(name, VirtualMachinePowerState.POWERED_OFF);

        while (iterating) {
            VirtualMachinePowerState state = getVirtualMachinePower(name);
            if (state != VirtualMachinePowerState.POWERED_OFF) {

                // Wait for power off.
                try {
                    TimeUnit.MILLISECONDS.sleep(5000L);
                } catch (Exception e) {
                    log.warn("Error waiting for power stop.");
                }

                if (attempts > 20) {
                    iterating = false;
                } else {
                    attempts++;
                }

            } else if (state == VirtualMachinePowerState.POWERED_OFF) {
                iterating = false;
                confirmed = true;
            } else {
                // If the VM doesn't exist or power-state cannot be retrieved.
                iterating = false;
                confirmed = false;
            }
        }
        return confirmed;
    }

    /**
     * URI for instance id.
     *
     * @param name name
     * @return url
     */
    public URI vmIdAsUri(String name) {
        return URI.create(context.endpoint + VsphereEndpoints.VSPHERE_VM.getPath().replace("{vm}", name));
    }
}