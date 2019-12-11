/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.authentication.switchuser.SwitchUserFilter;
import org.springframework.security.web.csrf.CookieCsrfTokenRepository;
import org.springframework.security.web.csrf.CsrfFilter;

import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.operation.RequestTrackingFilter;
import com.vmware.blockchain.services.profiles.Roles;

/**
 * Security Configuration for Helen.
 */
@Configuration
@EnableWebSecurity
@EnableGlobalMethodSecurity(prePostEnabled = true)
@ConditionalOnProperty(value = Constants.USE_CSP_AUTH, havingValue = "true")
public class CspSecurityConfig extends WebSecurityConfigurerAdapter {
    static final Logger logger = LogManager.getLogger(CspSecurityConfig.class);

    private OperationContext operationContext = new OperationContext();

    @Autowired
    TokenAuthenticationConfig tokenAuthenticationConfig;

    @Autowired
    TokenAuthenticationProvider tokenAuthenticationProvider;

    @Autowired
    VmbcBasicAuthProvider vmbcBasicAuthProvider;

    @Autowired
    private RestAuthenticationEntryPoint restAuthticationEntryPoint;

    @Autowired
    private TokenRefreshFilter tokenRefreshFilter;

    @Value("${vmbc.cookie.secure:true}")
    private boolean cookieSecure;

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        logger.info("Configure security with CSP enabled");

        // Disable CSRF (cross site request forgery)
        http.csrf().disable().sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS).and()
                // Session fixation is a problem with the ui due to concurrency
                .sessionManagement().sessionFixation().none().and()
                .authorizeRequests()
                .antMatchers("/api/auth/login", "/api/auth/token", "/api/agreements/1", "/", "/assets/**").permitAll()
                .antMatchers("/api/static/**").permitAll()
                .antMatchers("/api/oauth/login", "/api/oauth/oauth").permitAll()
                // anyone can look at the health
                .antMatchers("/api/management/health").permitAll()
                .antMatchers("/api/management/prometheus").permitAll()
                .antMatchers("/api/management/**").hasAnyRole(Roles.SYSTEM_ADMIN.getName())
                .anyRequest()
                .authenticated().and().exceptionHandling()
                .authenticationEntryPoint(restAuthticationEntryPoint)
                .and().anonymous().and().httpBasic();
        http.addFilterBefore(new TokenAuthenticationFilter(authenticationManager(),
                             tokenAuthenticationConfig, CookieCsrfTokenRepository.withHttpOnlyFalse()),
                             UsernamePasswordAuthenticationFilter.class);
        http.addFilterBefore(tokenRefreshFilter, TokenAuthenticationFilter.class);

        RequestTrackingFilter requestTrackingFilter = new RequestTrackingFilter(operationContext);

        http.addFilterBefore(requestTrackingFilter, TokenRefreshFilter.class);

        if (cookieSecure) {
            http.addFilterBefore(new FakeSslFilter(), CsrfFilter.class)
                    .addFilterAfter(new RemoveSslFilter(), SwitchUserFilter.class);
        }
    }

    @Override
    public void configure(WebSecurity web) throws Exception {
        // Allow access to auth, UI routing URLs, and UI assets, without authentication
        web.ignoring().antMatchers("/api/agreements/1").antMatchers("/api/auth/token", "/api/auth/login",
                                                                    "/api/static/**");
    }

    @Override
    protected void configure(AuthenticationManagerBuilder auth) throws Exception {
        auth.authenticationProvider(tokenAuthenticationProvider);
        auth.authenticationProvider(vmbcBasicAuthProvider);
    }

    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder(4);
    }

    @Bean
    public HelenUserDetailsService userDetails() {
        return new HelenUserDetailsService();
    }

}
