/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.lint;

import java.io.IOException;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.backoff.ExponentialBackOffPolicy;
import org.springframework.retry.policy.SimpleRetryPolicy;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponents;
import org.springframework.web.util.UriComponentsBuilder;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ErrorCodeType;
import com.vmware.blockchain.common.csp.CspAuthenticationHelper;
import com.vmware.blockchain.common.restclient.RestClientBuilder;
import com.vmware.blockchain.services.blockchains.Blockchain;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;

/**
 * Proxy requests to Log Intelligence.  We currently make queries using a Log Intelligence service user with admin
 * privileges, not the calling user's privileges.  We also need to limit the scope of the query to just the user's
 * consortium.
 */
@RestController
@RequestMapping(LintProxyController.LINT_API_LINK)
public class LintProxyController {

    public static final String LINT_API_LINK = "/api/lint";

    public static final Logger logger = LogManager.getLogger(LintProxyController.class);

    private AuthHelper authHelper;
    private CspAuthenticationHelper cspAuthHelper;
    private ReplicaService replicaService;
    private BlockchainService blockchainService;
    private String lintAuthToken;
    private String lintApiToken;
    private String cspUrl;

    private RestTemplate restTemplate;

    @Autowired
    public LintProxyController(AuthHelper authHelper, ReplicaService replicaService,
                               @Value("${lint.csp.url:https://console.cloud.vmware.com}") String cspUrl,
            @Value("${lint.apitoken:#null}") String lintApiToken, @Value("${lint.url}") String lintUrl) {
        this.authHelper = authHelper;
        this.cspUrl = cspUrl;
        this.cspAuthHelper = new CspAuthenticationHelper(cspUrl);
        this.lintApiToken = lintApiToken;
        this.replicaService = replicaService;

        // set up the RestTemplate to talk to LINT
        SimpleRetryPolicy retryPolicy = new SimpleRetryPolicy(3);
        ExponentialBackOffPolicy backOffPolicy = new ExponentialBackOffPolicy();
        // Start with a two second timeout
        backOffPolicy.setInitialInterval(TimeUnit.SECONDS.toMillis(2));
        backOffPolicy.setMaxInterval(TimeUnit.SECONDS.toMillis(15));
        // We are proxying a string that is already json, so we don't want a mapper on this template
        this.restTemplate = new RestClientBuilder()
                                .withBaseUrl(lintUrl)
                                .withRetryPolicy(retryPolicy)
                                .withBackoffPolicy(backOffPolicy)
                                .withNoObjectMapper()
                                .build();
    }

    private String getAuthToken() {
        // Note that there is a race condition here, since two threads may update this.
        // It turns out that this is not a problem, since both tokens will be valid for
        // basically the same amount of time.
        if (lintAuthToken == null || !cspAuthHelper.isTokenValidFor(lintAuthToken, 1000)) {
            lintAuthToken = cspAuthHelper.fetchAuthTokenFromRefreshToken(lintApiToken);
        }
        return lintAuthToken;
    }

    // If the body has a field "logQuery", fix the query.
    private String rewriteBody(String body, String blockchainId) {
        // Lint uses camelcase in json, so we use a different object mapper
        ObjectMapper mapper = new ObjectMapper();
        try {
            // Since we don't know how this body might change over time, deserialize as a map.
            Map<String, Object> map = mapper.readValue(body, new TypeReference<Map<String, Object>>() {});
            // might throw class cast exception.  Leave the body unchanged if so
            String query = (String) map.get("logQuery");
            if (query != null) {
                String whereClause = String.format("consortium_id = '%s'", blockchainId);
                SimpleSqlParser sql = new SimpleSqlParser(query);
                sql.addWhere(whereClause);
                map.put("logQuery", sql.toSql());
                return mapper.writeValueAsString(map);
            }
        } catch (IOException | ClassCastException e) {
            // If anything goes wrong, we simply use the original body
            logger.info(e.getMessage());
        }
        return body;
    }

    /**
     * Proxy the call to Log Intelligence.
     * Our calls to log intelligence must be made with a LINT account, thus the need to set
     * the Authorization header with a different access token. For a post call with a query
     * body, we need to add or modify the where clause to filter on the consortium ID.
     * There is an optional query param function that will be used to also filter on function.
     * By default this is "ethLogger".
     * We do the following:
     */
    @RequestMapping("/**")
    @PreAuthorize("@authHelper.isAuthenticated()")
    public ResponseEntity<String> proxyToLint(@RequestBody(required = false) String body,
            @RequestParam(name = "blockchain_id", required = false) String blockchainId,
            HttpMethod method, HttpServletRequest request, HttpServletResponse response) throws IllegalAccessException {

        if (HttpMethod.POST == method) {
            if (blockchainId == null) {
                logger.info("Missing blockchain_id request param in POST");
                throw new BadRequestException(ErrorCodeType.BAD_REQUEST_PARAM);
            }

            if (!authHelper.canAccessChain(UUID.fromString(blockchainId))) {
                // logger.info(String.format("Replica %s cannot access blockchain", replicaId));
                throw new BadRequestException(ErrorCodeType.CANNOT_ACCESS_BLOCKCHAIN);
            }
        }

        // get the request URI, and convert to lint relative request
        String path = request.getRequestURI();
        final String lintPath = path.substring(LINT_API_LINK.length());

        // Set the HttpHeaders to include the lint auth token
        final HttpHeaders headers = new HttpHeaders();
        headers.set(HttpHeaders.ACCEPT, MediaType.APPLICATION_JSON.toString());
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.set(HttpHeaders.AUTHORIZATION, "Bearer " + getAuthToken());

        // If this is a post, rewrite the body to handle consortium filter
        if (method == HttpMethod.POST) {
            Blockchain blockchain  = blockchainService.get(UUID.fromString(blockchainId));
            UUID consortiumId = blockchain.getConsortium();

            body = rewriteBody(body, consortiumId.toString());
        }

        // If there was a 'blockchain_id' query param, we need to remove it
        MultiValueMap<String, String> queryMap = new LinkedMultiValueMap<>();
        if (request.getQueryString() != null) {
            // request.queryString strips off the leading ?, but UriComponentsBuilder needs it.
            UriComponents comp = UriComponentsBuilder.fromUriString("?" + request.getQueryString()).build();
            queryMap.addAll(comp.getQueryParams());
            queryMap.remove("blockchain_id");
        }

        // Now build the URI to make the lint call.  The lint url is handled by the restTemplate.
        String uri = UriComponentsBuilder.newInstance()
                    .path(lintPath)
                    .queryParams(queryMap)
                    .build().toUriString();
        try {
            ResponseEntity<String> result =
                    restTemplate.exchange(uri, method, new HttpEntity<>(body, headers), String.class);
            return result;
        } catch (HttpClientErrorException ex) {
            logger.info("Proxy csp request {} failed, detail error {}", path,
                    ex.getResponseBodyAsString());
            return new ResponseEntity<>(ex.getResponseBodyAsString(), ex.getStatusCode());
        } catch (Exception ex) {
            logger.info("Proxy csp request {} failed", path, ex);
            return new ResponseEntity<>(ex.getLocalizedMessage(), HttpStatus.INTERNAL_SERVER_ERROR);
        }



    }

}
