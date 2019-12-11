/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.oauth2;

import java.io.IOException;
import java.time.Instant;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.auth.AuthUtil;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.InternalFailureException;
import com.vmware.blockchain.common.csp.CspCommon;
import com.vmware.blockchain.common.csp.CspConfig;
import com.vmware.blockchain.common.csp.CspConstants;

import io.micrometer.core.annotation.Timed;
import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * Handles the control for oauth2. Provide a login that redirects to the correct CSP discovery page to start the
 * process, and implement the callback that exchanges the code for token, and redirects to the intended target page.
 */
@RestController
@Timed("oauth")
public class Oauth2Controller {
    private static Logger logger = LogManager.getLogger(Oauth2Controller.class);

    private String ssUrl;

    private String clientId;

    private String clientSecret;

    private String vmbcOauthCallback;

    private String oauthCallbackUrl;

    private static RestTemplate restTemplate =
            Oauth2Helper.getOauthRestTemplate(3, TimeUnit.SECONDS.toMillis(1));

    @PostConstruct
    private void init() {
        oauthCallbackUrl = UriComponentsBuilder.fromUriString(ssUrl).path(vmbcOauthCallback).build().toUriString();
    }


    private CacheManager cacheManager;
    private AuthHelper authHelper;
    private CspConfig cspConfig;

    // This is the page to redirect a successful login. Currently hard-wired to prevent open redirect.
    private String targetUri;

    @Autowired
    public Oauth2Controller(CacheManager cacheManager, AuthHelper authHelper, CspConfig cspConfig,
                            @Value("${vmbc.url.public:http://test.com}") String ssUrl,
                            @Value("${vmbc.client.id:vmbc-client}") String clientId,
                            @Value("${vmbc.client.secret:vmbc-secret}") String clientSecret,
                            @Value(Constants.OAUTH_CALLBACK) String vmbcOauthCallback) {
        this.cacheManager = cacheManager;
        this.authHelper = authHelper;
        this.cspConfig = cspConfig;
        this.ssUrl = ssUrl;
        this.clientId = clientId;
        this.clientSecret = clientSecret;
        this.vmbcOauthCallback = vmbcOauthCallback;
        logger.info("Oauth initialized");
    }

    /**
     * Login page to start the oauth2 flow. Use this call to start the login. Pass in the path to the ultimate target
     * page for the flow. This redirects to the CSP discovery page with the client id, oauth callback, and target URI
     * filled in. org_link
     *
     * @param orgLink Org ID to log in as
     * @throws Exception redirect failure
     */
    @PreAuthorize("permitAll()")
    @RequestMapping(method = RequestMethod.GET, value = {Constants.AUTH_LOGIN})
    public void login(@RequestParam(name = Constants.CSP_ORG_LINK, required = false) String orgLink,
                      @RequestParam(name = Constants.CSP_INVITATION_LINK, required = false) String serviceInvitation,
                      @RequestParam(name = Oauth2CommonUtility.SESSION_CLEANED, required = false,
                              defaultValue = "false") boolean sessionRedirect,
                      @RequestParam(name = Constants.NEW_USER_PARAM, required = false) String userParam,
                      HttpServletRequest request, HttpServletResponse response) throws Exception {
        // VSKS-4629: If we have a session already, clean it up, and clear any authtokens
        // from the cache.
        HttpSession session = request.getSession(false);
        Oauth2AuthCodeRequestParams.Oauth2AuthCodeRequestParamsBuilder authCodeParamsBuilder =
                Oauth2AuthCodeRequestParams.builder();
        // If we have a session and have not been through the clean up...
        if (!sessionRedirect && session != null) {
            String token = (String) session.getAttribute(Constants.AUTH_HEADER_NAME);
            // evict our token if we have one
            if (token != null) {
                Cache cache = cacheManager.getCache(Constants.TOKEN_CACHE);
                cache.evict(token);
            }
            session.invalidate();
            // redirect back to this page, with the query param SESSION_CLEANED set to true
            // This forces the session invalidate to take effect, and forces a new session
            authCodeParamsBuilder.isSessionCleanRequired(true);
        } else {
            // Create a random state ID
            String state = UUID.randomUUID().toString();
            // and save it as an attribute in the session
            request.getSession().setAttribute(Oauth2CommonUtility.STATE_KEY, state);
            if (serviceInvitation != null) {
                request.getSession().setAttribute(Constants.CSP_INVITATION_LINK, serviceInvitation);
            }
            if (userParam != null) {
                request.getSession().setAttribute(Constants.NEW_USER_PARAM, userParam);
            }
            authCodeParamsBuilder.state(state).baseUrl(cspConfig.getCspUrl())
                    .relativePath(CspConstants.CSP_DISCOVERY_PAGE).clientId(clientId)
                    .redirectUri(oauthCallbackUrl).orgId(orgLink);
        }
        Oauth2Helper helper = new Oauth2Helper();
        helper.handleAuthorizationCodeRequest(request, response, authCodeParamsBuilder.build());
    }

    /**
     * Logout page. Invalidate the session and call the CSP logout method
     */
    @PreAuthorize("isAuthenticated()")
    @RequestMapping(method = RequestMethod.GET, value = {Constants.AUTH_LOGOUT})
    public void logout(HttpServletRequest request, HttpServletResponse response) throws Exception {
        // Replace the header token with get token
        String token = AuthUtil.getToken(request);

        // idToken is used by the logout call.  Need to get it here so we can close the session.
        String idToken = null;
        if (request.getSession(false) != null) {
            idToken = (String) request.getSession().getAttribute(Constants.TOKEN_ID);
        }

        // if either the idToken or the auth token are null, there is no session
        if (token == null || idToken == null) {
            String cspDiscoverypage = UriComponentsBuilder.fromUriString(cspConfig.getCspUrl())
                    .path(CspConstants.CSP_DISCOVERY_PAGE).build().toUriString();
            response.sendRedirect(cspDiscoverypage);
            return;
        }

        // we don't know if org or projects is controlling things, so get the cache manager
        Cache cache = cacheManager.getCache(Constants.CSP_TOKEN_CACHE);
        cache.evict(token);
        // invalidate the current session
        request.getSession().invalidate();

        // create the call to csp logout
        String logout = UriComponentsBuilder.fromUriString(cspConfig.getCspUrl()).path(CspConstants.CSP_LOGOUT).build()
                .toUriString();

        // In the new form, we POST to logout with the authToken, and receive a url to redirect to
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add(CspConstants.AUTH_HEADER_NAME, token);
        ResponseEntity<CspCommon.CspLogoutResponse> logoutResponse = null;
        HttpEntity<CspCommon.CspLogoutRequst> logoutRequest =
                new HttpEntity<>(new CspCommon.CspLogoutRequst(idToken), headers);

        try {
            logoutResponse =
                    restTemplate.exchange(logout, HttpMethod.POST, logoutRequest, CspCommon.CspLogoutResponse.class);
            String url = logoutResponse.getBody().getUrl();
            // Redirect to logout url
            response.setStatus(HttpServletResponse.SC_TEMPORARY_REDIRECT);
            response.setHeader("Location", url);
        } catch (HttpClientErrorException e) {
            logger.error("Error trying to logout: status{} body {}", e.getStatusCode(), e.getResponseBodyAsString());
            // return whatever error code we got
            response.sendError(e.getRawStatusCode(), e.getLocalizedMessage());
        }
    }


    /**
     * Oauth2 callback. This call is invoked through a redirect from the csp discovery page to the browser doing the
     * login. We need to exchange the code provided for a JWT token by calling the CSP auth token exchange method, with
     * the code provided, plus the redirect url, client id, and client secret.
     *
     * @param code Code returned from CSP discovery page
     * @param state Original target URL we will redirect to
     * @throws Exception kvStore failure
     */
    @PreAuthorize("permitAll()")
    @RequestMapping(method = RequestMethod.GET, value = Constants.OAUTH_CALLBACK)
    public void oauthCallback(@RequestParam String code, @RequestParam String state, HttpServletRequest request,
                              HttpServletResponse response) throws Exception {

        Oauth2AuthTokenRequestParams.Oauth2AuthTokenRequestParamsBuilder authTokenReqParamsBuilder =
                Oauth2AuthTokenRequestParams.builder();
        authTokenReqParamsBuilder.authorizationCode(code).state(state).clientId(clientId).clientSecret(clientSecret)
                .mediaType(MediaType.APPLICATION_FORM_URLENCODED).baseUrl(cspConfig.getCspUrl())
                .relativePath(CspConstants.CSP_OAUTH_TOKEN).grantType("authorization_code")
                .redirectUri(oauthCallbackUrl);

        Oauth2Helper helper = new Oauth2Helper();
        ResponseEntity<Oauth2AuthTokenResponse> tokenResponse =
                helper.handleAuthTokenRequest(request, response, authTokenReqParamsBuilder.build());

        if (tokenResponse.getStatusCode() != HttpStatus.OK) {
            throw new InternalFailureException(ErrorCode.CANNOT_EXCHANGE_TOKEN);
        }
        HttpSession session = request.getSession();
        Oauth2AuthTokenResponse authResponse = tokenResponse.getBody();

        // get the token, then save values in session
        long now = System.currentTimeMillis();
        String token = authResponse.getCspAuthToken();
        session.setAttribute(Constants.AUTH_HEADER_NAME, token);
        session.setAttribute(Constants.TOKEN_REFRESH, authResponse.getCspRefreshToken());
        session.setAttribute(Constants.TOKEN_ID, authResponse.getIdToken());
        session.setAttribute(Constants.TOKEN_EXPIRES_AT,
                             now + TimeUnit.SECONDS.toMillis(authResponse.getExpiresIn()));

        // create the build for the redirect.  If there is an invitation, this will redirect to the
        // invitaion handler, otherwize redirect to login-return
        UriComponentsBuilder redirectBuilder;
        if (session.getAttribute(Constants.CSP_INVITATION_LINK) != null) {
            redirectBuilder = UriComponentsBuilder.fromUriString(Constants.AUTH_INVITATION);
        } else {
            redirectBuilder = UriComponentsBuilder.fromUriString(Constants.AUTH_LOGIN_RETURN);
        }

        // Add the user flag, if there.
        String userparam = (String) session.getAttribute(Constants.NEW_USER_PARAM);
        if (userparam != null) {
            redirectBuilder.queryParam(Constants.NEW_USER_PARAM, userparam);
        }
        String redirect = redirectBuilder.build().toUriString();
        try {
            /*
                Removed code here that added the auth token as a query param to redirect.  I think I added that
                in originally for UI guys, but Venkat added a feature to leave it off.  Negotiate with Matt
                on how to get thee token.
             */
            response.sendRedirect(redirect);
        } catch (IOException e) {
            throw new InternalFailureException(ErrorCode.CANNOT_REDIRECT_TO_TARGET, redirect);
        }
    }

    // Classes to return data from various auth calls
    @Data
    @AllArgsConstructor
    static class CurrentToken {
        String authToken;
        String idToken;
        String email;
        long lastLogin;
    }

    /**
     * Return the current authtoken in the user's session. This is for the UI. We shouldn't be able to get to this call
     * if we aren't authenticated.
     *
     * @return current authtoken.
     */
    @PreAuthorize("isAuthenticated()")
    @RequestMapping(method = RequestMethod.GET, value = Constants.API_AUTH_TOKEN)
    public CurrentToken getAuthToken(HttpServletRequest request, HttpServletResponse response) {
        String idToken = "";
        HttpSession session = request.getSession(false);
        if (session != null) {
            idToken = (String) session.getAttribute(Constants.TOKEN_ID);
        }
        String email = authHelper.getDetails().getUsername();
        Instant n = authHelper.getDetails().getLastLogin();
        long lastLogin = n == null ? 0 : n.toEpochMilli();
        return new CurrentToken(authHelper.getAuthToken(), idToken, email, lastLogin);
    }


}
