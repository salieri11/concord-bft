/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
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

import com.vmware.blockchain.common.Constants;

import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.operation.RequestTrackingFilter;


/**
 * Security Configuration for Helen.
 */
@Configuration
@EnableWebSecurity
@EnableGlobalMethodSecurity(prePostEnabled = true)
@ConditionalOnProperty(value = Constants.USE_CSP_AUTH, havingValue = "false", matchIfMissing = true)
public class OldJwtSecurityConfig extends WebSecurityConfigurerAdapter {

    private OperationContext operationContext = new OperationContext();

    static final Logger logger = LogManager.getLogger(OldJwtSecurityConfig.class);

    @Autowired
    private JwtTokenProvider jwtTokenProvider;

    @Autowired
    private RestAuthenticationEntryPoint restAuthticationEntryPoint;

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        logger.info("Configuring security with internal JWT provider");
        // Disable CSRF (cross site request forgery)
        http.csrf().disable().sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS).and()
                .authorizeRequests()
                // anyone can look at the health
                .antMatchers("/api/management/health").permitAll()
                .antMatchers("/api/auth/login", "/api/auth/token", "/api/agreements/1", "/", "/assets/**")
                .permitAll().anyRequest()
                .authenticated().and().exceptionHandling()
                .authenticationEntryPoint(restAuthticationEntryPoint).and().anonymous().and().httpBasic();
        // Non-csp version of token filter
        JwtTokenFilter customFilter = new JwtTokenFilter(jwtTokenProvider);
        http.addFilterBefore(customFilter, UsernamePasswordAuthenticationFilter.class);

        RequestTrackingFilter requestTrackingFilter = new RequestTrackingFilter(operationContext);
        http.addFilterBefore(requestTrackingFilter, JwtTokenFilter.class);

    }

    @Override
    public void configure(WebSecurity web) throws Exception {
        // Allow access to auth, UI routing URLs, and UI assets, without authentication
        web.ignoring().antMatchers("/api/agreements/1").antMatchers("/api/auth/token", "/api/auth/login");
    }

    @Override
    protected void configure(AuthenticationManagerBuilder auth) throws Exception {
        auth.userDetailsService(userDetails()).passwordEncoder(passwordEncoder());
    }

    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder(4);
    }

    @Bean
    public JwtTokenProvider tokenProvider() {
        return new JwtTokenProvider();
    }

    @Bean
    public HelenUserDetailsService userDetails() {
        return new HelenUserDetailsService();
    }

}
