/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp.api.client;

import java.math.BigInteger;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.RSAPublicKeySpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import javax.validation.constraints.NotNull;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.util.CollectionUtils;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponents;
import org.springframework.web.util.UriComponentsBuilder;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Stopwatch;
import com.vmware.blockchain.base.auth.Role;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.ForbiddenException;
import com.vmware.blockchain.common.InternalFailureException;
import com.vmware.blockchain.common.csp.CspAuthenticationInterceptor;
import com.vmware.blockchain.common.csp.CspCommon;
import com.vmware.blockchain.common.csp.CspCommon.CspRefLinkResult;
import com.vmware.blockchain.common.csp.CspCommon.CspServiceLinkResult;
import com.vmware.blockchain.common.csp.CspCommon.CspServiceRole;
import com.vmware.blockchain.common.csp.CspCommon.CspServiceRoleStatus;
import com.vmware.blockchain.common.csp.CspCommon.CspTokenPublicKeyResponse;
import com.vmware.blockchain.common.csp.CspConstants;
import com.vmware.blockchain.common.csp.CspUtils;
import com.vmware.blockchain.common.csp.exception.CspApiClientErrorException;
import com.vmware.blockchain.common.csp.exception.CspApiException;
import com.vmware.blockchain.common.csp.exception.CspException;
import com.vmware.blockchain.common.csp.exception.CspTaskFailureException;
import com.vmware.blockchain.common.csp.exception.CspTaskWaitTimeoutException;
import com.vmware.blockchain.common.restclient.RestClientBuilder;

import io.micrometer.core.annotation.Timed;
import lombok.Data;

/**
 * Client for CSP APIs. To create a new instance of this client, please use the {@link CspApiClientBuilder}.
 * All methods use client auth by default. This should be injected via
 * {@link com.vmware.blockchain.common.csp.CspAuthenticationInterceptor}
 */
public class CspApiClient {

    static final String CSP = "/csp";
    //Use this rest template if calls require a service owner auth access
    private RestTemplate serviceOwnerAuthRestTemplate;
    private RestTemplate clientCredentialsAuthRestTemplate;
    private RestTemplate noAuthRestTemplate;

    private ObjectMapper objectMapper;

    private static final Logger logger = LogManager.getLogger(CspApiClient.class);

    //CSP API base url.
    static final String CSP_GATEWAY = "/csp/gateway";

    // CSP MessageSubscription api paths.
    static final String PATH_MESSAGESUBSCRIPTION = CSP_GATEWAY + "/po/api";
    static final String PATH_MESSAGESUBSCRIPTION_DEFINITIONS = PATH_MESSAGESUBSCRIPTION
                                                               + "/v1/subscribers/{subscriber}/subscriptions";
    // CSP Service LifeCycle api paths.
    static final String PATH_SERVICE_LIFECYCLE = CSP_GATEWAY + "/slc/api";
    static final String PATH_ALL_SERVICE_DEFINITIONS = PATH_SERVICE_LIFECYCLE + "/definitions";
    static final String PATH_SUBSCRIPTIONS = PATH_SERVICE_LIFECYCLE + "/service-subscriptions";

    //CSP Billing api paths
    static final String PATH_BILLING = CSP_GATEWAY + "/billing/api";
    static final String PATH_V2_BILLING = CSP_GATEWAY + "/billing/api/v2";
    static final String PATH_BUSINESS_PLANS = PATH_BILLING + "/business-plans";
    public static final String PATH_BILLING_SERVICE_DEF = "/service-definitions/{serviceDefinitionId}";
    @Deprecated
    static final String PATH_OFFERS = PATH_BILLING + PATH_BILLING_SERVICE_DEF + "/offers";
    static final String PATH_V2_OFFERS = PATH_V2_BILLING + PATH_BILLING_SERVICE_DEF + "/offers";

    @Deprecated
    static final String PATH_OFFER_RATES = PATH_BILLING + PATH_BILLING_SERVICE_DEF + "/rates";
    static final String PATH_V2_OFFER_RATES = PATH_V2_BILLING + PATH_BILLING_SERVICE_DEF + "/rates";

    @Deprecated
    public static final String PATH_ADDONS = PATH_BILLING + PATH_BILLING_SERVICE_DEF + "/addons";
    public static final String PATH_V2_ADDONS = PATH_V2_BILLING + PATH_BILLING_SERVICE_DEF + "/addons";

    static final String PATH_OFFER_SUBSCRIPTIONS = PATH_BILLING + "/subscriptions";
    static final String PATH_OFFER_A_SUBSCRIPTION = PATH_BILLING + "/subscriptions/{subscriptionId}";

    //CSP Account/User management api paths.
    static final String PATH_ACCOUNT_MANAGEMENT = CSP_GATEWAY + "/am/api";
    static final String PATH_ORGS = PATH_ACCOUNT_MANAGEMENT + "/orgs";
    static final String PATH_LOGGED_IN_USER = PATH_ACCOUNT_MANAGEMENT + "/loggedin/user";
    static final String PATH_USER = PATH_ACCOUNT_MANAGEMENT + "/users";

    static final String PATH_SERVICE_INVITATION = PATH_SERVICE_LIFECYCLE + "/service-invitations";

    static final String PATH_ORDER_INVITATION = PATH_BILLING + "/order-invitations";

    static final String PATH_BILLING_PROMOTIONS = PATH_BILLING + "/promotions";

    static final String PATH_BILLING_PROMOTION_ASSIGNMENT = PATH_BILLING_PROMOTIONS + "/assignment";

    // CSP Usage api paths
    static final String PATH_USAGE_RECORDS_SUBMISSION = CSP_GATEWAY + "/billing/api/usages/records";

    // CSP Funds API Path.
    static final String PATH_ORG_FUNDS = PATH_BILLING + "/orgs/{orgId}/payment-methods";

    // CSP Currency API Path.
    static final String PATH_ORG_CURRENCY = PATH_BILLING + "/orgs/{orgId}/currency";

    //Query Params
    static final String QP_EXPAND_RESULTS = "expand";

    // Offer constant
    public static final String OFFER_ADDONS_TYPE_USAGE = "usage";
    public static final String OFFER_CURRENCY_USD = "USD";

    //Commerce api endpoints
    static final String COMMERCE_BASE_URL = CSP_GATEWAY + "/commerce/api/v1";
    static final String SUBSCRIPTIONS = COMMERCE_BASE_URL + "/subscriptions";
    static final String A_SUBSCRIPTION = SUBSCRIPTIONS + "/{subscriptionId}";
    static final String COMMERCE_API_SERVICE_DEF_BASE =
            COMMERCE_BASE_URL + "/service-definitions/{serviceDefinitionId}";


    /**
     * Create a {@link CspApiClient} instance with the given {@link RestTemplate}.
     * Instantiated via {@link CspApiClientBuilder}
     *
     * @param restTemplate - The {@link RestTemplate} to use for Csp api calls.
     * @deprecated - Please use
     */
    @Deprecated
    protected CspApiClient(RestTemplate restTemplate) {
        this.serviceOwnerAuthRestTemplate = restTemplate;
    }

    /**
     * Generate a client by providing auth providers for different apis. Currently CSP honors 2 kinds of tokens
     * for service to service api calls.
     * <ol>
     *     <li>Service owner token - generated either by refresh_token or the access key of the service owner user.
     *     In DI this uses username/password</li>
     *     <li>client_credentials grant - generated using clientId and client secret.</li>
     * </ol>
     * @param restClientBuilder - The restclient builder with options - ex. retry, logging.
     * @param serviceOwnerAuthProvider - The auth interceptor that can provide service owner tokens.
     * @param clientCredentialsAuthProvider - The auth interceptor that can provide client credentials token.
     */
    protected CspApiClient(RestClientBuilder restClientBuilder, CspAuthenticationInterceptor serviceOwnerAuthProvider,
                           CspAuthenticationInterceptor clientCredentialsAuthProvider) {
        this.clientCredentialsAuthRestTemplate =
                restClientBuilder.withAuthentication(clientCredentialsAuthProvider).build();
        this.serviceOwnerAuthRestTemplate = restClientBuilder.withAuthentication(serviceOwnerAuthProvider).build();
        this.noAuthRestTemplate = restClientBuilder.build();
        this.objectMapper = restClientBuilder.getObjectMapper();
    }

    /** Logged in user API calls. **/

    /**
     * Retrieve user's orgs from CSP for given authtoken.
     *
     * @return list of org Ids.
     */
    @Timed("csp.api")
    public CspCommon.CspOrgsResult getCspOrgsForLoggedInUser(String authToken) {
        HttpEntity<String> httpEntity = new HttpEntity<>(getLoggedinUserAuthHeader(authToken));
        ResponseEntity<CspCommon.CspOrgsResult> orgResponse =
                serviceOwnerAuthRestTemplate
                    .exchange(UriComponentsBuilder.fromPath(PATH_LOGGED_IN_USER).path("/orgs?expand=1").toUriString(),
                              HttpMethod.GET,
                              httpEntity,
                              CspCommon.CspOrgsResult.class);
        return orgResponse.getBody();
    }

    /**
     * Get logged in user information.
     * @param authToken - Token of the logged in user.
     * @return - the user.
     */
    @Timed("csp.api")
    public CspCommon.CspUser getUser(String authToken) {
        String userUri = UriComponentsBuilder.fromPath(CspConstants.CSP_USER_VALIDATION_API).build().toUriString();

        HttpEntity<String> httpEntity = new HttpEntity<>(getLoggedinUserAuthHeader(authToken));

        ResponseEntity<CspCommon.CspUser> userEntity =
                serviceOwnerAuthRestTemplate.exchange(userUri, HttpMethod.GET, httpEntity, CspCommon.CspUser.class);
        return userEntity.getBody();
    }

    /**
     * Get service roles for a user from org.
     * @param orgId - The org id for which roles need to be looked up.
     * @param authToken - auth token of the user.
     * @return CspUserServiceRolesResponse.
     */
    @Timed("csp.api")
    public CspCommon.CspUserServiceRolesResponse getOrgServiceRoles(UUID orgId, String userName, String authToken) {
        HttpEntity<CspCommon.CspUserServiceRolesResponse> request =
                new HttpEntity<>(getLoggedinUserAuthHeader(authToken));
        return serviceOwnerAuthRestTemplate.exchange(
                PATH_USER + "/" + userName + "/orgs/{orgId}/service-roles", HttpMethod.GET,
                request, CspCommon.CspUserServiceRolesResponse.class, orgId).getBody();
    }

    /**
     * Get service roles for a user from org.
     *
     * @param orgId     - The org id for which roles need to be looked up.
     * @param authToken - auth token of the user.
     * @return CspUserServiceRolesResponse.
     */
    @Timed("csp.api")
    public CspCommon.CspUserServiceRolesResponse getLoggedInUserOrgServiceRoles(UUID orgId,
                                                                                String authToken) {
        HttpEntity<CspCommon.CspUserServiceRolesResponse> request =
                new HttpEntity<>(getLoggedinUserAuthHeader(authToken));
        return serviceOwnerAuthRestTemplate.exchange(
                PATH_LOGGED_IN_USER + "/orgs/{orgId}/service-roles", HttpMethod.GET,
                request, CspCommon.CspUserServiceRolesResponse.class, orgId).getBody();
    }

    /**
     * Patch service roles for a user for an org.
     */
    @Timed("csp.api")
    public void patchOrgServiceRoles(UUID orgId, String userName, CspCommon.CspPatchServiceRolesRequest body) {
        HttpEntity<CspCommon.CspPatchServiceRolesRequest> request =
                new HttpEntity<>(body);
        serviceOwnerAuthRestTemplate.exchange(
                PATH_USER + "/" + userName + "/orgs/{orgId}/service-roles", HttpMethod.PATCH,
                request, CspCommon.CspUserServiceRolesResponse.class, orgId);
    }

    /**
     * Patch service roles for a user for an org, with a specific authtoken.
     */
    @Timed("csp.api")
    public void patchOrgServiceRoles(String token, UUID orgId, String userName,
                                     CspCommon.CspPatchServiceRolesRequest body) {
        HttpEntity<CspCommon.CspPatchServiceRolesRequest> request =
                new HttpEntity<>(body, getLoggedinUserAuthHeader(token));
        noAuthRestTemplate.exchange(
                PATH_USER + "/" + userName + "/orgs/{orgId}/service-roles", HttpMethod.PATCH,
                request, CspCommon.CspUserServiceRolesResponse.class, orgId);
    }

    /**
     * Get Csp org details.
     *
     * @param orgId org Id.
     * @param authToken - the auth token of the user.
     * @return Org with details.
     */
    @Timed("csp.api")
    public CspCommon.CspOrg getCspOrg(String orgId, String authToken) {
        HttpEntity<String> request = new HttpEntity<>(getLoggedinUserAuthHeader(authToken));
        return serviceOwnerAuthRestTemplate
                .exchange(PATH_ORGS + "/{orgId}", HttpMethod.GET, request, CspCommon.CspOrg.class,
                          orgId)
            .getBody();
    }

    /**
     * Create an  org in Csp.
     * @param authToken - The auth token of the user to which the org will be associated.
     * @param orgName - name of the org.
     */
    @Timed("csp.api")
    public CspCommon.CspOrgResult createDefaultCspOrg(String orgName, String authToken) {
        HttpHeaders headers = getLoggedinUserAuthHeader(authToken);
        CspCommon.CspOrg org = new CspCommon.CspOrg();
        org.setDisplayName(orgName);
        HttpEntity<CspCommon.CspOrg> entity = new HttpEntity<>(org, headers);
        return serviceOwnerAuthRestTemplate.exchange(PATH_ORGS, HttpMethod.POST, entity, CspCommon.CspOrgResult.class)
                .getBody();
    }

    /** End of Logged In User API calls. **/

    /** User Management API calls. **/

    /**
     * Create a user in CSP.
     *
     * @param cspUser Details of the user to be created.
     * @return String - link to the new user.
     */
    @Timed("csp.api")
    public CspRefLinkResult createUser(CspCommon.CspUser cspUser) {
        // create user in CSP
        ResponseEntity<CspRefLinkResult> exchange =
                serviceOwnerAuthRestTemplate
                        .postForEntity(PATH_ACCOUNT_MANAGEMENT + "/users/", cspUser, CspRefLinkResult.class);
        logger.info("User created in CSP with username:" + cspUser.getUsername());
        return exchange.getBody();
    }

    /** End of User Management API calls. **/

    /** CSP Auth Token API calls. ***/


    /**
     * Login a user using the accesskey.
     *
     * @param accessKey provided for a given user
     * @return Authorization Token if successful
     * @throws ForbiddenException if access is not valid
     */
    @Timed("csp.api")
    public CspCommon.CspLoginResponse loginAccessKey(String accessKey) {
        CspCommon.CspAccessKey key = new CspCommon.CspAccessKey();
        key.setKey(accessKey);
        HttpEntity<CspCommon.CspAccessKey> loginReq = new HttpEntity<>(key, new HttpHeaders());
        try {
            ResponseEntity<CspCommon.CspLoginResponse> loginResponse =
                    serviceOwnerAuthRestTemplate.exchange(CspConstants.CSP_LOGIN_API_KEY, HttpMethod.POST, loginReq,
                                                          CspCommon.CspLoginResponse.class);
            return loginResponse.getBody();
        } catch (HttpClientErrorException e) {
            throw new ForbiddenException(ErrorCode.USER_NOT_AUTHENTICATED, "service client");
        }
    }

    /**
     * Checks token validity with CSP. Returns true/false.
     *
     * @param token bearer token
     */
    @Timed("csp.api")
    public boolean isValidToken(String token) {
        if (StringUtils.isBlank(token)) {
            return false;
        }
        String validateTokenUri = UriComponentsBuilder.fromPath(CspConstants.CSP_VALIDATE_TOKEN).buildAndExpand(token)
                .toString();
        HttpHeaders headers = new HttpHeaders();
        HttpEntity<String> httpEntity = new HttpEntity<>(headers);
        ResponseEntity<String> exchange =
                serviceOwnerAuthRestTemplate.exchange(validateTokenUri, HttpMethod.GET, httpEntity, String.class);
        return Boolean.valueOf(exchange.getBody());
    }

    /**
     * Validates the given bearer token, throws exception if not valid.
     * Unlike validateToken, this simply checks whether or not the
     * token is valid, and does not attempt to retrieve user or org data,
     * and does not cache the value.
     *
     * @param authToken bearer token
     */
    public void validateBearerToken(String authToken) {
        try {
            if (!isValidToken(authToken)) {
                throw new BadCredentialsException("Auth token is not valid");
            }
        } catch (CspApiException ex) {
            if (ex.getHttpStatus() == HttpStatus.UNAUTHORIZED) {
                logger.info("Unauthorized token, exiting");
            } else {
                logger.info("Exception while validating token ", ex);
            }
            throw new BadCredentialsException(ex.getMessage());
        }
    }

    /**
     * Fetch jwt signing key for verifying tokens.
     */
    public PublicKey getJwtPublicKey() throws Exception {
        String uri = UriComponentsBuilder.fromPath(PATH_ACCOUNT_MANAGEMENT).pathSegment("auth", "token-public-key")
                .toUriString();
        HttpHeaders headers = new HttpHeaders();
        HttpEntity<String> httpEntity = new HttpEntity<>(headers);
        ResponseEntity<CspTokenPublicKeyResponse> exchange =
                serviceOwnerAuthRestTemplate.exchange(uri, HttpMethod.GET, httpEntity, CspTokenPublicKeyResponse.class);
        String publicKeyPem = exchange.getBody().getValue();
        publicKeyPem = publicKeyPem.replace("-----BEGIN PUBLIC KEY-----\n", "");
        publicKeyPem = publicKeyPem.replace("\n-----END PUBLIC KEY-----", "");
        logger.info("Retrieved public key from CSP alg {} issuer {}", exchange.getBody().getAlg(),
                exchange.getBody().getIssuer());

        X509EncodedKeySpec spec = new X509EncodedKeySpec(Base64.decodeBase64(publicKeyPem));
        KeyFactory kf = KeyFactory.getInstance("RSA");
        return kf.generatePublic(spec);
    }

    /**
     * Get a list of jwk (Json Web Key) from CSP to validate a jwt. Most of the logic is from
     * https://github.com/auth0/jwks-rsa-java/blob/master/src/main/java/com/auth0/jwk/Jwk.java
     *
     * @return - A map of jwk and their kid.
     */
    public Map<String, PublicKey> getJwtPublicKeys() {
        //Get the list of keys from CSP
        String uri = UriComponentsBuilder
                .fromPath(PATH_ACCOUNT_MANAGEMENT)
                .pathSegment("auth", "token-public-key")
                .queryParam("format", "jwks")
                .toUriString();
        CspCommon.CspJwksResponse jwks =
                serviceOwnerAuthRestTemplate.getForObject(uri, CspCommon.CspJwksResponse.class);
        List<CspCommon.CspJwksResponse.Jwk> keys = jwks.getKeys();

        if (CollectionUtils.isEmpty(keys)) {
            return Collections.emptyMap();
        }

        Map<String, PublicKey> kidPkMap = new HashMap<>();
        //Build a kid to key map.
        for (CspCommon.CspJwksResponse.Jwk key : keys) {
            if (!CspConstants.SUPPORTED_PK_ALGORITHM.equals(key.getKty())) {
                logger.error("Unsupported key format {} for kid: {}", key.getKty(), key.getKid());
                throw new InternalFailureException(
                        "Unsupported key type " + key.getKty() + " for kid " + key.getKid());
            }
            try {
                KeyFactory kf = KeyFactory.getInstance(CspConstants.SUPPORTED_PK_ALGORITHM);
                BigInteger modulus = new BigInteger(1, Base64.decodeBase64(key.getModulus()));
                BigInteger exponent = new BigInteger(1, Base64.decodeBase64(key.getExponent()));
                kidPkMap.put(key.getKid(), kf.generatePublic(new RSAPublicKeySpec(modulus, exponent)));
            } catch (InvalidKeySpecException e) {
                logger.error("InvalidKeySpecException while trying to create PK for kid {}", key.getKid(), e);
                throw new InternalFailureException("Invalid public key" + e.getMessage());
            } catch (NoSuchAlgorithmException e) {
                logger.error("NoSuchAlgorithmException while trying to create PK for kid {}", key.getKid(), e);
                throw new InternalFailureException("Invalid algorithm to generate key", e);
            }
        }
        logger.info("Retrieved list of keys with kids {}", kidPkMap.keySet());
        return kidPkMap;
    }

    /** End of CSP Auth Token API calls. ***/

    /** TODO: removing Subscription API calls. **/

    /** End of Subscription API calls. **/

    /** CSP notification Subscription API calls. **/
    /**
     * Returns service definitions.
     * @param subscriberId  - identifies the clientId and orgId  requesting the notifications
     * @param body  - SubscriptionStatusUpdateNotificationsRequest instance to specify message type and callback
     * @return SubscriptionStatusUpdateNotificationsResponse.
     */
    @Timed("csp.api")
    public CspCommon.SubscriptionStatusUpdateNotificationsResponse subscribeToNotification(
            String subscriberId,
            CspCommon.SubscriptionStatusUpdateNotificationsRequest body) {
        HttpHeaders headers = new HttpHeaders();
        HttpEntity<CspCommon.SubscriptionStatusUpdateNotificationsRequest> request =
                new HttpEntity<>(body, headers);
        ResponseEntity<CspCommon.SubscriptionStatusUpdateNotificationsResponse> result;
        result = serviceOwnerAuthRestTemplate.exchange(
                PATH_MESSAGESUBSCRIPTION_DEFINITIONS, HttpMethod.POST,
                request, CspCommon.SubscriptionStatusUpdateNotificationsResponse.class, subscriberId);
        return result.getBody();
    }

    /**
     * Get current notification subscriptions from CSP.
     * @param subscriberId  - identifies the clientId and orgId  requesting the notifications
     * @return MessageSubscriptionResult.
     */
    @Timed("csp.api")
    public CspCommon.Subscriptions getNotificationSubscriptions(
            String subscriberId) {
        ResponseEntity<CspCommon.Subscriptions> result;
        result = serviceOwnerAuthRestTemplate.exchange(
                PATH_MESSAGESUBSCRIPTION_DEFINITIONS, HttpMethod.GET,
                new HttpEntity<>(""), CspCommon.Subscriptions.class, subscriberId);
        return result.getBody();
    }

    /** end of CSP notification Subscription API calls. **/

    /** Service Registration/Lifecycle API calls. **/

    /**
     * Returns service definitions.
     * @return A {@link com.vmware.blockchain.common.csp.CspCommon.ServiceDefinitions}.
     */
    @Timed("csp.api")
    public CspCommon.ServiceDefinitions getServiceDefinitions() {
        return serviceOwnerAuthRestTemplate.getForObject(PATH_ALL_SERVICE_DEFINITIONS,
                                                         CspCommon.ServiceDefinitions.class);
    }

    /**
     * Returns service definitions.
     *
     * @return A {@link com.vmware.blockchain.common.csp.CspCommon.ServiceDefinitions}.
     */
    @Timed("csp.api")
    public CspCommon.ServiceDefinitions getServiceDefinitionsExpanded() {
        return serviceOwnerAuthRestTemplate.getForObject(
                UriComponentsBuilder.fromPath(PATH_ALL_SERVICE_DEFINITIONS)
                        .queryParam(QP_EXPAND_RESULTS, 1)
                        .build().toUriString(), CspCommon.ServiceDefinitions.class);
    }


    /**
     * Get Service definition by serviceDefinitionLink.
     * @param serviceDefinitionLink  - Link to the service definition.
     * @return ServiceDefinition.
     */
    @Timed("csp.api")
    public CspCommon.ServiceDefinition getServiceDefinition(String serviceDefinitionLink) {
        return serviceOwnerAuthRestTemplate.getForObject(serviceDefinitionLink, CspCommon.ServiceDefinition.class);
    }

    /**
     * Register a service definition with csp.
     * @param serviceDefinitionYaml  - Yaml definition of the service.
     * @return CspServiceDefinitionResult - Result of the registration process.
     */
    @Timed("csp.api")
    public CspCommon.CspServiceDefinitionResult registerServiceDefinition(String serviceDefinitionYaml) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.TEXT_PLAIN);
        HttpEntity<String> serviceRegisterRequest = new HttpEntity<>(serviceDefinitionYaml, headers);
        return serviceOwnerAuthRestTemplate
                .postForObject(PATH_SERVICE_LIFECYCLE + "/definitions", serviceRegisterRequest,
                               CspCommon.CspServiceDefinitionResult.class);
    }

    /**
     * Register a service with csp.
     * @param serviceInstanceRegistrationRequest - Definition of the service instance.
     * @return returns the result.
     */
    public String registerService(CspCommon.ServiceInstance serviceInstanceRegistrationRequest) {
        HttpEntity<CspCommon.ServiceInstance> entity = new HttpEntity<>(serviceInstanceRegistrationRequest);
        ResponseEntity<CspServiceLinkResult> response =
                serviceOwnerAuthRestTemplate
                        .exchange(PATH_SERVICE_LIFECYCLE + "/lifecycle/register", HttpMethod.POST, entity,
                                  CspServiceLinkResult.class);
        return response.getBody().getServiceLink();
    }

    /**
     * Update a service's definition.
     *
     * @param updateRequest         - Request.
     * @param serviceDefinitionLink - The service definition link.
     * @return a CspServiceDefinitionResult instance.
     */
    @Timed("csp.api")
    public CspCommon.CspServiceDefinitionResult updateServiceDefinition(
            CspCommon.ServiceDefinitionUpdateRequest updateRequest, String serviceDefinitionLink) {
        HttpEntity<CspCommon.ServiceDefinitionUpdateRequest> requestHttpEntity = new HttpEntity<>(updateRequest);
        ResponseEntity<CspCommon.CspServiceDefinitionResult> result =
                serviceOwnerAuthRestTemplate.exchange(serviceDefinitionLink, HttpMethod.PATCH, requestHttpEntity,
                                                      CspCommon.CspServiceDefinitionResult.class);
        return result.getBody();
    }

    /**
     * Register service roles.
     * CSP PATCH doesn't create - so attempt to create all roles - those will likely return 409 - then attempt to patch
     */
    @Timed("csp.api")
    public void registerServiceRoles(String serviceDefinitionLink,
                                     Set<Role> roles) {
        for (Role role : roles) {
            CspServiceRole cspServiceRole =
                    new CspServiceRole(role.getName(), role.getDisplayName(), role.isDefaultRole(),
                                       CspServiceRoleStatus.ACTIVE.toString(), role.isHidden());
            HttpEntity<CspCommon.CspServiceRole> entity = new HttpEntity<>(cspServiceRole);
            try {
                serviceOwnerAuthRestTemplate
                        .postForEntity(serviceDefinitionLink + "/service-roles", entity, CspRefLinkResult.class);
            } catch (CspApiClientErrorException e) {
                if (e.getHttpStatus() != HttpStatus.OK && e.getHttpStatus() != HttpStatus.CONFLICT) {
                    logger.warn("Couldn't set service role {} status {}", role.getName(), e.getHttpStatus());
                }
            }
        }
        // now patch existing roles with any new info
        patchServiceRoles(serviceDefinitionLink, roles);
    }

    @Data
    private static class PatchServiceRoles {
        List<CspServiceRole> serviceRoles;
    }

    /**
     * patch service roles.
     */
    @Timed("csp.api")
    public void patchServiceRoles(String serviceDefinitionLink,
                                  Set<Role> roles) {
        logger.info("Patching service roles for service definition {} roles {}", serviceDefinitionLink, roles);
        PatchServiceRoles request = new PatchServiceRoles();
        request.serviceRoles = new ArrayList<>();
        for (Role role : roles) {
            CspServiceRole cspServiceRole =
                    new CspServiceRole(role.getName(), role.getDisplayName(), role.isDefaultRole(),
                                       CspServiceRoleStatus.ACTIVE.toString(), role.isHidden());
            request.serviceRoles.add(cspServiceRole);
        }
        HttpEntity<PatchServiceRoles> entity = new HttpEntity<>(request);
        try {
            serviceOwnerAuthRestTemplate.patchForObject(serviceDefinitionLink, entity, CspRefLinkResult.class);
        } catch (CspApiClientErrorException e) {
            if (e.getHttpStatus() != HttpStatus.OK && e.getHttpStatus() != HttpStatus.CONFLICT) {
                logger.warn("Couldn't PATCH service roles for service definition {} status {}",
                        serviceDefinitionLink, e.getHttpStatus());
            }
        }
    }

    /**
     * Generate invitations for a service. More than one invitation can be generated with this api call.
     *
     * @param invitationRequest - To generate invitations, you need to specify the CspServiceInvitationRequest. Please
     *                          refer the docs on that object for more info on the fields.
     */
    @Timed("csp.api")
    public CspCommon.CspRefLinksResponse generateServiceInvites(
            CspCommon.CspServiceInvitationRequest invitationRequest) {
        logger.info("Generating {} invitations for service {} ", invitationRequest.getNumberOfInvitationsToGenerate(),
                    invitationRequest.getServiceDefinitionLink());
        return serviceOwnerAuthRestTemplate
                .postForObject(PATH_SERVICE_INVITATION, invitationRequest, CspCommon.CspRefLinksResponse.class);
    }

    /**
     * Generate invitations for a sales-led order for the service.
     * More than one invitation can be generated with this api call.
     *
     * @param invitationRequest - To generate invitations, you need to specify the CspServiceInvitationRequest. Please
     *                          refer the docs on that object for more info on the fields.
     */
    @Timed("csp.api")
    public CspCommon.CspRefLinksResponse generateSalesOrderInvite(
            CspCommon.CspServiceInvitationRequest invitationRequest) {
        logger.info("Generating {} order invitations for service {} for order {}",
                    invitationRequest.getNumberOfInvitationsToGenerate(),
                    invitationRequest.getServiceDefinitionLink(),
                    invitationRequest.getOrderId());
        return serviceOwnerAuthRestTemplate
                .postForObject(PATH_ORDER_INVITATION, invitationRequest, CspCommon.CspRefLinksResponse.class);
    }

    /**
     * Get the invitation details from CSP.
     *
     * @param invitationLink - The invitation link from csp.
     * @return a CspServiceInvitation object containing the service invitation details.
     */
    @Timed("csp.api")
    public CspCommon.CspServiceInvitation getInvitation(String invitationLink) {
        String formattedLink = formatInvitationLink(invitationLink);
        logger.debug("Getting from invitation link {} ", formattedLink);
        return serviceOwnerAuthRestTemplate
                .getForObject(UriComponentsBuilder.fromPath(formattedLink).build().toUriString(),
                              CspCommon.CspServiceInvitation.class);
    }

    /** End of Service Registration/Lifecycle API calls. **/

    /** oAuth Client API calls. **/

    /**
     * Register the service with CSP as an oauth client.
     * @param oauthClient - the client to be registered.
     * @return Result of the registration.
     */
    @Timed("csp.api")
    public CspCommon.CspRegisterClientResponse registerOauthClient(CspCommon.OauthClient oauthClient) {
        logger.info("Registering {} as an oauth client", oauthClient.getId());
        return serviceOwnerAuthRestTemplate.postForObject(CspConstants.CSP_REGISTER_OAUTH_CLIENT, oauthClient,
                                                          CspCommon.CspRegisterClientResponse.class);
    }

    /**
     * Patch the oauth clients with scopes to add or remove.
     * @param scopes Scopes to add and remove, and the service definition link.
     */
    @Timed("csp.api")
    public void patchOauthClient(CspCommon.OauthClientScopes scopes) {
        logger.info("Patch Oauth clients for service {}", scopes.getServiceDefinitionLink());
        serviceOwnerAuthRestTemplate.patchForObject(CspConstants.CSP_REGISTER_OAUTH_CLIENT, scopes, Void.class);
    }

    /**
     * Get the details of the given oauth client.
     * @param clientId client id.
     * @return Result of get.
     */
    @Timed("csp.api")
    public CspCommon.CspRegisterClientResponse getOauthClient(String clientId) {
        logger.info("get oauth client {}", clientId);
        return serviceOwnerAuthRestTemplate.getForObject(
                UriComponentsBuilder.fromPath(CspConstants.CSP_REGISTER_OAUTH_CLIENT).path("/{clientId}").build()
                .toUriString(), CspCommon.CspRegisterClientResponse.class, clientId);
    }

    /**
     * Delete the current oauth client.  This will allow us to update the oauth client
     * on server initialization.
     */
    @Timed("csp.api")
    public void deleteOauthClient(String clientId) {
        logger.info("Deleting oauth client {}", clientId);
        serviceOwnerAuthRestTemplate.delete(
                UriComponentsBuilder.fromPath(CspConstants.CSP_REGISTER_OAUTH_CLIENT).path("/{clientId}").build()
                        .toUriString(), clientId);
        logger.info("Deleted oauth client {}", clientId);
    }

    /**
     * Generate a new access key for the logged in user.
     * The first access key needs to be generated via csp user management UI.
     * @param authToken - The auth token of the user.

     */
    public CspCommon.CspAccessKey createNewAccessKey(String userName, String authToken) {
        HttpEntity<String> accessKeyRequest = new HttpEntity<>(getLoggedinUserAuthHeader(authToken));
        logger.info("Generating new access key for user {} ", userName);
        return serviceOwnerAuthRestTemplate.postForObject(
                UriComponentsBuilder.fromPath(PATH_LOGGED_IN_USER)
                        .path("/access-keys").build().toUriString(), accessKeyRequest, CspCommon.CspAccessKey.class);
    }

    /**
     * Delete the access key of the logged in user.
     *
     */
    public void deleteAccessKey(String userName, String authToken) {
        HttpEntity<String> request = new HttpEntity<>(getLoggedinUserAuthHeader(authToken));
        String uri = UriComponentsBuilder.fromPath(PATH_LOGGED_IN_USER)
                .path("/access-keys").build().toUriString();
        try {
            serviceOwnerAuthRestTemplate.exchange(uri, HttpMethod.DELETE, request, CspCommon.CspAccessKey.class);
        } catch (CspApiClientErrorException e) {
            if (e.getHttpStatus() == HttpStatus.NOT_FOUND) {
                logger.info("Access key does not exist for user {} ", userName);
            } else {
                logger.warn("Unable to delete access-key", e);
            }
        } catch (Exception e) {
            logger.warn("Unable to delete access-key for {}", userName, e);

        }
    }
    /** End of oAuth Client API calls. **/

    /** Additional Helper Methods. **/

    /**
     * Get the status of a task submitted to CSP.
     *
     * @param taskUrl - The url of the task. This is generally returned when you submit a task to CSP as refLink.
     */
    @Timed("csp.api")
    public CspCommon.CspSubscriptionTaskResponse getTaskResponse(String taskUrl) {
        ResponseEntity<CspCommon.CspSubscriptionTaskResponse> taskResponse =
                serviceOwnerAuthRestTemplate
                        .exchange(taskUrl, HttpMethod.GET, null, CspCommon.CspSubscriptionTaskResponse.class);
        return taskResponse.getBody();
    }

    /**
     * Convenience method for {@link #waitForTask(String, TimeUnit, long, long, CspCommon.CspTaskStatus...)}.
     * The following defaults are passed
     * <br> unit - {@link TimeUnit#SECONDS}
     * <br> sleepDuration - {@link CspConstants#DEFAULT_WAIT_IN_SECONDS}
     * <br> timeout - {@link CspConstants#DEFAULT_TIMEOUT_IN_SECONDS}
     * <br> successStatuses = {@link CspCommon.CspTaskStatus#COMPLETE}
     *
     * @param taskUrl The taskUrl to poll against. This is generally the refLink returned when you schedule a task.
     */
    public void waitForTaskCompletion(String taskUrl) {
        waitForTask(taskUrl, TimeUnit.SECONDS, CspConstants.DEFAULT_WAIT_IN_SECONDS,
                    CspConstants.DEFAULT_TIMEOUT_IN_SECONDS,
                    CspCommon.CspTaskStatus.COMPLETE);
    }

    /**
     * Wait for a CSP task to complete. Please be aware that this is a {@link Thread#sleep(long)}.
     * The wait will be aborted if CSP_TASK_STATUS_FAILED is returned from CSP.
     *
     * @param cspTaskLink     The task URL on CSP. This is usually the refLink returned when you submit a task.
     * @param unit            Unit of time. For e.g. {@link TimeUnit#SECONDS}
     * @param sleepDuration   How long to sleep before polling. Sleeps for duration times unit. Uses {@link
     *                        TimeUnit#sleep(long)}
     * @param waitTimeout     How long before I abort. Measures duration w.r.t to the unit. Will throw {@link
     *                        CspTaskWaitTimeoutException} after timeout
     * @param successStatuses List of statuses that mean success. Refer @{@link CspCommon.CspTaskStatus}
     * @throws CspApiException - In case of api errors from Csp. Refer {@link CspApiException#getHttpStatus()} to
     *                         determine http status.
     * @throws CspException    - In other exception cases.
     */
    public void waitForTask(@NotNull String cspTaskLink, @NotNull TimeUnit unit, long sleepDuration, long waitTimeout,
                            @NotNull CspCommon.CspTaskStatus... successStatuses) {

        Set<String> success = new HashSet<>(
                Arrays.stream(successStatuses).map(s -> s.getStatusText()).collect(Collectors.toList()));

        Stopwatch stopwatch = Stopwatch.createStarted();
        // Wait till timeout or Success/Failure from CSP.
        while (true) {
            try {
                if (stopwatch.elapsed(unit) > waitTimeout) {
                    stopwatch.stop();
                    logger.debug("Current stopwatch {} ", stopwatch.elapsed(TimeUnit.MILLISECONDS));
                    logger.warn("Wait for task url {} exceeds {} {}. Aborting", cspTaskLink, waitTimeout,
                                unit.toString());
                    throw new CspTaskWaitTimeoutException(cspTaskLink, waitTimeout, unit);
                }
                CspCommon.CspSubscriptionTaskResponse taskResponse = getTaskResponse(cspTaskLink);
                String taskStage = taskResponse.getTaskStage();
                logger.info("Task stage for task url {} after {} {} is {} ", cspTaskLink, stopwatch.elapsed(unit),
                            unit.toString(),
                            taskStage);
                // Check if the task failed.
                if (CspCommon.CspTaskStatus.FAILED.getStatusText().equals(taskStage)) {
                    throw new CspTaskFailureException(cspTaskLink, taskResponse.getTaskStage(),
                                                      taskResponse.getFailureMessage());
                }
                //Check if the taskStage is one of the success statuses. if not, wait.
                if (success.contains(taskStage)) {
                    return;
                } else {
                    try {
                        unit.sleep(sleepDuration);
                    } catch (InterruptedException e) {
                        logger
                                .error("The thread was interrupted while waiting for csp task {} to complete",
                                       cspTaskLink,
                                       e);
                        throw new CspException(
                                "Task wait for taskUrl " + cspTaskLink + " was interrupted abruptly", e);
                    }
                }
            } catch (HttpClientErrorException e) {
                logger.warn("Bad request to CSP. Error while calling task status", e);
                throw new CspApiException(e.getStatusCode(), "Client error while calling csp taskUrl" + cspTaskLink,
                                          e);
            } catch (HttpServerErrorException e) {
                logger.warn("Server error on CSP. Error while calling task status", e);
                throw new CspApiException(e.getStatusCode(), "Server error while calling csp taskUrl" + cspTaskLink,
                                          e);
            }
        }
    }

    /**
     * Get the list of offers for a service definition. This is currently the list of offers that are present in SDP
     * for a service.
     *
     * @param serviceDefinitionLink - The link to the service definition.
     * @param nextPageToken         - The next token for pagination (not implemented yet)
     * @param previousPageToken     - The token for the previous page (not implemented yet)
     * @return - A list of offers.
     */
    @Deprecated
    @Timed("csp.api")
    public CspCommon.PaginatedResponse<CspCommon.Offer> getOffers(String serviceDefinitionLink, String nextPageToken,
                                                                  String previousPageToken) {
        ParameterizedTypeReference<CspCommon.PaginatedResponse<CspCommon.Offer>> type =
            new ParameterizedTypeReference<CspCommon.PaginatedResponse<CspCommon.Offer>>() {
            };
        ResponseEntity<CspCommon.PaginatedResponse<CspCommon.Offer>> exchange =
                clientCredentialsAuthRestTemplate
                        .exchange(PATH_OFFERS, HttpMethod.GET, null, type, CspUtils.getSelfId(serviceDefinitionLink));
        return exchange.getBody();
    }

    /**
     * Get the list of offers for a service definition. This is currently the list of offers that are present in SDP
     * for a service.
     *
     * @param serviceDefinitionId - The Id of the service definition.
     * @param orgLink             - The org ref link (Format /csp/gateway/am/api/orgs/{id}).
     * @param currency            - Currency.
     * @return - A list of offers.
     */
    @Timed("csp.api")
    public List<CspCommon.OfferExpandedDto> getOffersV2(
            String serviceDefinitionId, String orgLink, String currency) {
        UriComponentsBuilder builder = UriComponentsBuilder.fromPath(PATH_V2_OFFERS);
        if (null != orgLink) {
            builder = builder.queryParam("orgLink", orgLink);
        }
        if (null != currency) {
            builder = builder.queryParam("currency", currency);
        }

        ParameterizedTypeReference<List<CspCommon.OfferExpandedDto>> type =
                new ParameterizedTypeReference<List<CspCommon.OfferExpandedDto>>() { };
        ResponseEntity<List<CspCommon.OfferExpandedDto>> exchange =
                clientCredentialsAuthRestTemplate.exchange(
                        builder.build().toUriString(), HttpMethod.GET, null, type, serviceDefinitionId);
        return exchange.getBody();
    }

    /**
     * Get details of a specific offer.
     *
     * @param serviceDefinitionLink - The service which owns the offer.
     * @param offerName             - Name of the offer.
     * @param offerVersion          Version of the offer.
     * @return an Offer object.
     */
    @Deprecated
    @Timed("csp.api")
    public CspCommon.Offer getOfferDetail(String serviceDefinitionLink, String offerName, String offerVersion) {
        UriComponents uriComponents = UriComponentsBuilder.fromPath(PATH_OFFERS).queryParam("name", offerName)
                .queryParam("version", offerVersion)
                .buildAndExpand(CspUtils.getSelfId(serviceDefinitionLink));
        return clientCredentialsAuthRestTemplate.getForObject(uriComponents.toUriString(), CspCommon.Offer.class);
    }

    /**
     * Get details of a specific offer.
     *
     * @param serviceDefinitionId - The service which owns the offer.
     * @param offerName             - Name of the offer.
     * @param offerVersion          - Version of the offer.
     * @param orgLink               - The org ref link (Format /csp/gateway/am/api/orgs/{id}).
     * @param currency              - Currency.
     * @return an Offer object.
     */
    @Timed("csp.api")
    public CspCommon.OfferExpandedDto getOfferDetail(
            String serviceDefinitionId, String offerName, String offerVersion, String orgLink, String currency) {
        UriComponentsBuilder builder = UriComponentsBuilder.fromPath(PATH_V2_OFFERS)
                .queryParam("name", offerName)
                .queryParam("version", offerVersion);
        if (null != orgLink) {
            builder = builder.queryParam("orgLink", orgLink);
        }
        if (null != currency) {
            builder = builder.queryParam("currency", currency);
        }

        ParameterizedTypeReference<List<CspCommon.OfferExpandedDto>> type =
                new ParameterizedTypeReference<List<CspCommon.OfferExpandedDto>>() { };

        ResponseEntity<List<CspCommon.OfferExpandedDto>> exchange =
                clientCredentialsAuthRestTemplate.exchange(
                        builder.build().toUriString(), HttpMethod.GET, null, type, serviceDefinitionId);

        List<CspCommon.OfferExpandedDto> offers = exchange.getBody();
        if (1 != offers.size()) {
            logger.warn("CSP returns {} offer details for serviceDefinitionId {}, offerName {}, and offerVersion {}.",
                        offers.size(), serviceDefinitionId, offerName, offerVersion);
            if (offers.size() < 1) {
                throw new CspException("CSP returns no offer detail for serviceDefinitionId " + serviceDefinitionId
                                       + ", offerName " + offerName + ", and offerVersion " + offerVersion + ".");
            }
        }
        return offers.get(0);
    }

    /* End CSP Commerce apis */

    private HttpHeaders getLoggedinUserAuthHeader(final String authToken) {
        HttpHeaders headers = new HttpHeaders();
        headers.add(CspConstants.AUTH_HEADER_NAME, authToken);
        headers.add(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE);
        return headers;
    }

    /**
     * Convert org id to org link.
     * @param orgId Org Id
     * @return Org Link
     */
    public static String cvtOrgIdToLink(UUID orgId) {
        return UriComponentsBuilder.fromPath(CspApiClient.PATH_ORGS).pathSegment(orgId.toString()).toUriString();
    }

    /**
     * Convert service definition link to service definition id.
     * @param serviceDefinitionLink service definition link
     * @return service definition id
     */
    public static String cvtServiceDefinitionLinkToId(String serviceDefinitionLink) {
        return CspUtils.getSelfId(serviceDefinitionLink);
    }

    /**
     * This is a hack to format the invitation link the right way. The link that csp returns is not directly consumable
     * by the getInvitation api. (duh!)
     * Basically this formats csp/slc/xxxx or csp/billing/xxxx to /csp/gateway/slc/xxxx or csp/gateway/billing/xxxx.
     * @param invitationLink - the link to be formatted.
     * @return - Formatted link if it can be formatted.
     */
    public String formatInvitationLink(String invitationLink) {
        if (StringUtils.isBlank(invitationLink)) {
            return invitationLink;
        }
        if (invitationLink.contains(CSP_GATEWAY)) {
            return invitationLink;
        }
        return invitationLink.replaceFirst(CSP, CSP_GATEWAY);
    }
}
