/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.List;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.EntityModificationException;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.HelenException;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.common.UnauthorizedException;
import com.vmware.blockchain.security.HelenUserDetails;
import com.vmware.blockchain.security.JwtTokenProvider;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * A servlet for handling the user authentication flow of helen. This servlet is just added for temporary usage. Actual
 * authentication will be done with CSP. Do NOT rely on this servlet for primary authentication method.
 */
@Controller
public class UserAuthenticator {

    private static final Logger logger = LogManager.getLogger(UserAuthenticator.class);

    private UserService userService;

    private KeystoreService keystoreService;

    private ProfilesService prm;

    private PasswordEncoder passwordEncoder;

    private JwtTokenProvider jwtTokenProvider;


    @Getter
    @Setter
    @NoArgsConstructor
    private static class LoginRequest {
        private String email;
        private String password;
    }

    @Getter
    @Setter
    @NoArgsConstructor
    @JsonInclude(value =  Include.NON_EMPTY)
    private static class LoginResponse {
        // login response potentially has all the fields of User
        private UUID userId;
        private String userName;
        private String firstName;
        private String lastName;
        private String email;
        private String role;
        private String password;
        private long lastLogin;
        private UUID organizationId;
        private Boolean authenticated;
        private String token;
        private String refreshToken;
        private Long tokenExpires;
        private String walletAddress;
        private String error;

        // Convenience function for dealing with the user fields
        public void setUser(User user) {
            this.userId = user.getId();
            this.userName = user.getName();
            this.firstName = user.getFirstName();
            this.lastName = user.getLastName();
            this.email = user.getEmail();
            this.role = user.getServiceRoles().isEmpty() ? null : user.getServiceRoles().get(0).getName();
            this.lastLogin = user.getLastLogin() ==  null ? 0 : user.getLastLogin().toEpochMilli();
            this.organizationId = user.getOrganization();
        }

    }

    @Autowired
    public UserAuthenticator(UserService userService, ProfilesService prm,
            PasswordEncoder passwordEncoder, JwtTokenProvider jwtTokenProvider,
            KeystoreService keystoreService) {
        this.userService = userService;
        this.prm = prm;
        this.keystoreService = keystoreService;
        this.passwordEncoder = passwordEncoder;
        this.jwtTokenProvider = jwtTokenProvider;
    }


    // TODO: This is not a proper way to authenticate the user. We have plans to
    // authenticate every user via CSP, however that integration will take time
    // and till then some way of authentication is needed. Hence, we have added
    // this temporary (and not very secure) login feature. Remove this and
    // authenticate every user with CSP as soon as possible
    @RequestMapping(method = RequestMethod.POST, path = "/api/auth/login")
    protected ResponseEntity<LoginResponse> doPost(@RequestBody LoginRequest request) {
        LoginResponse loginResponse = new LoginResponse();
        try {
            String password = request.getPassword();
            String email = request.getEmail();
            User u = userService.getByEmail(email);
            if (!passwordEncoder.matches(password, u.getPassword())) {
                throw new UnauthorizedException(ErrorCode.BAD_LOGIN_REQUEST);
            }

            // need to get another image of user
            List<Keystore> keystores = keystoreService.getWalletsForUser(u.getId());
            if (keystores.size() > 0) {
                loginResponse.setWalletAddress(keystores.get(0).getAddress());
            } else {
                loginResponse.setWalletAddress("");
            }
            loginResponse.setUser(u);
            loginResponse.setAuthenticated(true);
            loginResponse.setToken(jwtTokenProvider.createToken(u));
            loginResponse.setRefreshToken(jwtTokenProvider.createRefreshToken(u));
            loginResponse.setTokenExpires(jwtTokenProvider.getValidityInMilliseconds());
            // This needs to be after we have copied the old user data
            // Needs to set the Security context before we save this login
            SecurityContextHolder.getContext()
                    .setAuthentication(jwtTokenProvider.getAuthentication(loginResponse.getToken()));
            prm.loginUser(u);
        } catch (NotFoundException e) {
            throw new UnauthorizedException(ErrorCode.BAD_LOGIN_REQUEST);
        } catch (EntityModificationException e) {
            throw new BadRequestException(e, ErrorCode.BAD_REQUEST);
        }

        return new ResponseEntity<>(loginResponse, HttpStatus.OK);
    }

    @Getter
    @Setter
    @NoArgsConstructor
    private static class TokenRequest {
        private String refreshToken;
    }

    @RequestMapping(value = "/api/auth/token", method = RequestMethod.POST)
    protected ResponseEntity<LoginResponse> refreshToken(@RequestBody TokenRequest request) {
        LoginResponse loginResponse = new LoginResponse();

        try {
            String token = request.getRefreshToken();

            if (token == null) {
                throw new BadRequestException(ErrorCode.BAD_TOKEN);
            }
            Authentication auth = jwtTokenProvider.getAuthentication(token);
            SecurityContextHolder.getContext().setAuthentication(auth);
            HelenUserDetails details = (HelenUserDetails) auth.getDetails();

            String email = details.getUsername();
            User u = userService.getByEmail(email);
            String newToken = jwtTokenProvider.createToken(u);
            String refreshToken = jwtTokenProvider.createRefreshToken(u);
            loginResponse.setToken(newToken);
            loginResponse.setRefreshToken(refreshToken);
            loginResponse.setTokenExpires(jwtTokenProvider.getValidityInMilliseconds());
        } catch (HelenException e) {
            throw new BadRequestException(e, ErrorCode.BAD_REQUEST);
        }

        return new ResponseEntity<>(loginResponse, HttpStatus.OK);

    }

    @RequestMapping(method = RequestMethod.POST, path = "/api/auth/change-password")
    @PreAuthorize("isAuthenticated()")
    protected ResponseEntity<UsersGetResponse> doChangePassword(@RequestBody LoginRequest request) {
        try {
            if (request.getEmail() == null || request.getPassword() == null) {
                throw new BadRequestException(ErrorCode.BAD_LOGIN_REQUEST);
            }

            User u = userService.getByEmail(request.getEmail());
            String password = request.getPassword();

            if (passwordEncoder.matches(password, u.getPassword())) {
                throw new BadRequestException(ErrorCode.BAD_PASSWORD_CHANGE);
            }
            String enPw = passwordEncoder.encode(password);
            return new ResponseEntity<>(prm.getReponse(prm.changePassword(u, enPw)), HttpStatus.OK);

        } catch (EntityModificationException e) {
            throw new BadRequestException(e, ErrorCode.BAD_REQUEST);
        } catch (NotFoundException e) {
            throw new BadRequestException(ErrorCode.BAD_LOGIN_REQUEST);
        }
    }
}

