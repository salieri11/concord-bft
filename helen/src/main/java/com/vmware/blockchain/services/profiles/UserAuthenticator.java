/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static com.vmware.blockchain.services.profiles.UsersApiMessage.EMAIL_LABEL;
import static com.vmware.blockchain.services.profiles.UsersApiMessage.PASSWORD_LABEL;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.vmware.athena.Athena;
import com.vmware.blockchain.common.AthenaProperties;
import com.vmware.blockchain.common.HelenException;
import com.vmware.blockchain.common.UserModificationException;
import com.vmware.blockchain.connections.AthenaConnectionPool;
import com.vmware.blockchain.security.JwtTokenProvider;
import com.vmware.blockchain.services.BaseServlet;
import com.vmware.blockchain.services.ethereum.ApiHelper;


/**
 * A servlet for handling the user authentication flow of helen. This servlet is just added for temporary usage. Actual
 * authentication will be done with CSP. Do NOT rely on this servlet for primary authentication method.
 */
@Controller
public class UserAuthenticator extends BaseServlet {

    private static final Logger logger = LogManager.getLogger(UserAuthenticator.class);

    private UserRepository userRepository;

    private ProfilesRegistryManager prm;

    private PasswordEncoder passwordEncoder;

    private JwtTokenProvider jwtTokenProvider;

    @Autowired
    public UserAuthenticator(AthenaProperties config, AthenaConnectionPool athenaConnectionPool,
            UserRepository userRepository, ProfilesRegistryManager prm, PasswordEncoder passwordEncoder,
            JwtTokenProvider jwtTokenProvider) {
        super(config, athenaConnectionPool);
        this.userRepository = userRepository;
        this.prm = prm;
        this.passwordEncoder = passwordEncoder;
        this.jwtTokenProvider = jwtTokenProvider;
    }

    // TODO: This is not a proper way to authenticate the user. We have plans to
    // authenticate every user via CSP, however that integration will take time
    // and till then some way of authentication is needed. Hence, we have added
    // this temporary (and not very secure) login feature. Remove this and
    // authenticate every user with CSP as soon as possible
    @RequestMapping(method = RequestMethod.POST, path = "/api/auth/login")
    protected ResponseEntity<JSONAware> doPost(@RequestBody String requestBody) {
        JSONParser parser = new JSONParser();
        HttpStatus responseStatus;
        JSONObject responseJson;
        // Need to create an init user if one doesn't exist
        // so we can login.
        prm.createUserIfNotExist();
        try {
            JSONObject requestJson = (JSONObject) parser.parse(requestBody);
            if (requestJson.containsKey(EMAIL_LABEL) && requestJson.containsKey(PASSWORD_LABEL)) {

                String password = requestJson.get(PASSWORD_LABEL).toString();
                String email = requestJson.get(EMAIL_LABEL).toString();
                User u = userRepository.findUserByEmail(email).get();

                if (passwordEncoder.matches(password, u.getPassword())) {
                    JSONObject user = prm.loginUser(email);
                    responseStatus = HttpStatus.OK;
                    responseJson = user;

                    List<Roles> roles = u.getRoles();
                    String token = jwtTokenProvider.createToken(email, u.getRoles());
                    responseJson.put("token", token);
                    String refreshToken = jwtTokenProvider.createRefreshToken(email, u.getRoles());
                    responseJson.put("refresh_token", refreshToken);
                    responseJson.put("token_expires", jwtTokenProvider.validityInMilliseconds);
                } else {
                    responseStatus = HttpStatus.UNAUTHORIZED;
                    responseJson = new JSONObject();
                }
            } else {
                responseJson = ApiHelper.errorJson("email or password " + "field missing");
                responseStatus = HttpStatus.BAD_REQUEST;
            }
        } catch (ParseException | UserModificationException e) {
            responseStatus = HttpStatus.BAD_REQUEST;
            responseJson = ApiHelper.errorJson(e.getMessage());
        }

        return new ResponseEntity<>(responseJson, standardHeaders, responseStatus);
    }

    @RequestMapping(value = "/api/auth/token", method = RequestMethod.POST)
    protected ResponseEntity<JSONAware> refreshToken(@RequestBody String requestBody) {
        JSONObject responseJson = new JSONObject();
        JSONParser parser = new JSONParser();
        HttpStatus responseStatus;

        try {
            JSONObject requestJson = (JSONObject) parser.parse(requestBody);
            String token = null;

            if (requestJson.get("refresh_token") != null) {
                token = requestJson.get("refresh_token").toString();
            }

            if (token != null && jwtTokenProvider.validateToken(token)) {
                responseStatus = HttpStatus.OK;
                Authentication auth = token != null ? jwtTokenProvider.getAuthentication(token) : null;
                SecurityContextHolder.getContext().setAuthentication(auth);
                String email = jwtTokenProvider.getEmail(token);
                User u = userRepository.findUserByEmail(email).get();
                String newToken = jwtTokenProvider.createToken(email, u.getRoles());
                responseJson.put("token", newToken);
                String refreshToken = jwtTokenProvider.createRefreshToken(email, u.getRoles());
                responseJson.put("refresh_token", refreshToken);
                responseJson.put("token_expires", jwtTokenProvider.validityInMilliseconds);
            } else {
                responseStatus = HttpStatus.BAD_REQUEST;
            }
        } catch (ParseException | HelenException e) {
            responseStatus = HttpStatus.BAD_REQUEST;
            responseJson = ApiHelper.errorJson(e.getMessage());
        }

        return new ResponseEntity<>(responseJson, standardHeaders, responseStatus);

    }

    @RequestMapping(method = RequestMethod.POST, path = "/api/auth/change-password")
    protected ResponseEntity<JSONAware> doChangePassword(@RequestBody String requestBody) {
        JSONParser parser = new JSONParser();
        HttpStatus responseStatus;
        JSONObject responseJson;

        try {
            JSONObject requestJson = (JSONObject) parser.parse(requestBody);
            if (requestJson.containsKey(EMAIL_LABEL) && requestJson.containsKey(PASSWORD_LABEL)) {

                String email = requestJson.get(EMAIL_LABEL).toString();
                User u = userRepository.findUserByEmail(email).get();
                String password = requestJson.get(PASSWORD_LABEL).toString();

                if (passwordEncoder.matches(password, u.getPassword())) {
                    responseJson = ApiHelper.errorJson("Can't use same password!");
                    responseStatus = HttpStatus.BAD_REQUEST;
                } else {
                    String enPw = passwordEncoder.encode(password);
                    responseStatus = HttpStatus.OK;
                    responseJson = prm.changePassword(email, enPw);
                }

            } else {
                responseJson = ApiHelper.errorJson("email or password " + "field missing");
                responseStatus = HttpStatus.BAD_REQUEST;
            }
        } catch (ParseException | UserModificationException e) {
            responseStatus = HttpStatus.BAD_REQUEST;
            responseJson = ApiHelper.errorJson(e.getMessage());
        }

        return new ResponseEntity<>(responseJson, standardHeaders, responseStatus);
    }

    @Override
    protected JSONAware parseToJson(Athena.AthenaResponse athenaResponse) {
        return null;
    }
}
