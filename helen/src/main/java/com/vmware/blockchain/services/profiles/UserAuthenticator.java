/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static com.vmware.blockchain.services.profiles.UsersAPIMessage.EMAIL_LABEL;
import static com.vmware.blockchain.services.profiles.UsersAPIMessage.PASSWORD_LABEL;

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
import com.vmware.blockchain.common.CustomException;
import com.vmware.blockchain.connections.AthenaConnectionPool;
import com.vmware.blockchain.security.JwtTokenProvider;
import com.vmware.blockchain.services.BaseServlet;
import com.vmware.blockchain.services.ethereum.APIHelper;


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
        JSONObject responseJSON;
        // Need to create an init user if one doesn't exist
        // so we can login.
        prm.createUserIfNotExist();
        try {
            JSONObject requestJSON = (JSONObject) parser.parse(requestBody);
            if (requestJSON.containsKey(EMAIL_LABEL) && requestJSON.containsKey(PASSWORD_LABEL)) {

                String password = requestJSON.get(PASSWORD_LABEL).toString();
                String email = requestJSON.get(EMAIL_LABEL).toString();
                User u = userRepository.findUserByEmail(email).get();

                if (passwordEncoder.matches(password, u.getPassword())) {
                    JSONObject user = prm.loginUser(email);
                    responseStatus = HttpStatus.OK;
                    responseJSON = user;

                    List<Roles> roles = u.getRoles();
                    String token = jwtTokenProvider.createToken(email, u.getRoles());
                    responseJSON.put("token", token);
                    String refreshToken = jwtTokenProvider.createRefreshToken(email, u.getRoles());
                    responseJSON.put("refresh_token", refreshToken);
                    responseJSON.put("token_expires", jwtTokenProvider.validityInMilliseconds);
                } else {
                    responseStatus = HttpStatus.UNAUTHORIZED;
                    responseJSON = new JSONObject();
                }
            } else {
                responseJSON = APIHelper.errorJSON("email or password " + "field missing");
                responseStatus = HttpStatus.BAD_REQUEST;
            }
        } catch (ParseException | UserModificationException e) {
            responseStatus = HttpStatus.BAD_REQUEST;
            responseJSON = APIHelper.errorJSON(e.getMessage());
        }

        return new ResponseEntity<>(responseJSON, standardHeaders, responseStatus);
    }

    @RequestMapping(value = "/api/auth/token", method = RequestMethod.POST)
    protected ResponseEntity<JSONAware> refreshToken(@RequestBody String requestBody) {
        JSONObject responseJSON = new JSONObject();
        JSONParser parser = new JSONParser();
        HttpStatus responseStatus;

        try {
            JSONObject requestJSON = (JSONObject) parser.parse(requestBody);
            String token = null;

            if (requestJSON.get("refresh_token") != null) {
                token = requestJSON.get("refresh_token").toString();
            }

            if (token != null && jwtTokenProvider.validateToken(token)) {
                responseStatus = HttpStatus.OK;
                Authentication auth = token != null ? jwtTokenProvider.getAuthentication(token) : null;
                SecurityContextHolder.getContext().setAuthentication(auth);
                String email = jwtTokenProvider.getEmail(token);
                User u = userRepository.findUserByEmail(email).get();
                String newToken = jwtTokenProvider.createToken(email, u.getRoles());
                responseJSON.put("token", newToken);
                String refreshToken = jwtTokenProvider.createRefreshToken(email, u.getRoles());
                responseJSON.put("refresh_token", refreshToken);
                responseJSON.put("token_expires", jwtTokenProvider.validityInMilliseconds);
            } else {
                responseStatus = HttpStatus.BAD_REQUEST;
            }
        } catch (ParseException | CustomException e) {
            responseStatus = HttpStatus.BAD_REQUEST;
            responseJSON = APIHelper.errorJSON(e.getMessage());
        }

        return new ResponseEntity<>(responseJSON, standardHeaders, responseStatus);

    }

    @RequestMapping(method = RequestMethod.POST, path = "/api/auth/change-password")
    protected ResponseEntity<JSONAware> doChangePassword(@RequestBody String requestBody) {
        JSONParser parser = new JSONParser();
        HttpStatus responseStatus;
        JSONObject responseJSON;

        try {
            JSONObject requestJSON = (JSONObject) parser.parse(requestBody);
            if (requestJSON.containsKey(EMAIL_LABEL) && requestJSON.containsKey(PASSWORD_LABEL)) {

                String email = requestJSON.get(EMAIL_LABEL).toString();
                User u = userRepository.findUserByEmail(email).get();
                String password = requestJSON.get(PASSWORD_LABEL).toString();

                if (passwordEncoder.matches(password, u.getPassword())) {
                    responseJSON = APIHelper.errorJSON("Can't use same password!");
                    responseStatus = HttpStatus.BAD_REQUEST;
                } else {
                    String enPw = passwordEncoder.encode(password);
                    responseStatus = HttpStatus.OK;
                    responseJSON = prm.changePassword(email, enPw);
                }

            } else {
                responseJSON = APIHelper.errorJSON("email or password " + "field missing");
                responseStatus = HttpStatus.BAD_REQUEST;
            }
        } catch (ParseException | UserModificationException e) {
            responseStatus = HttpStatus.BAD_REQUEST;
            responseJSON = APIHelper.errorJSON(e.getMessage());
        }

        return new ResponseEntity<>(responseJSON, standardHeaders, responseStatus);
    }

    @Override
    protected JSONAware parseToJSON(Athena.AthenaResponse athenaResponse) {
        return null;
    }
}
