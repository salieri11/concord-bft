/**
 * Copyright 2018 VMware, all rights reserved.
 *
 */

package Servers;

import org.springframework.context.annotation.Bean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;

import security.JwtTokenFilterConfigurer;
import security.JwtTokenProvider;
import security.MyUserDetails;
import security.RestAuthenticationEntryPoint;


@Configuration
@ComponentScan(basePackages = {"security"})
@EnableWebSecurity
public class WebSecurityConfig extends WebSecurityConfigurerAdapter {

    @Autowired
    private JwtTokenProvider jwtTokenProvider;

    @Autowired
    private RestAuthenticationEntryPoint restAuthticationEntryPoint;


    @Override
    protected void configure(HttpSecurity http) throws Exception {
        // Disable CSRF (cross site request forgery)
        http.csrf().disable().sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS).and().authorizeRequests()
                .antMatchers("/api/auth/login", "/api/auth/token", "/api/agreements/1", "/", "/assets/**", "/swagger/**").permitAll()
                .antMatchers("/api/users").hasAnyAuthority("CONSORTIUM_ADMIN", "SYSTEM_ADMIN", "ORG_ADMIN")
                .antMatchers("/api/athena/**").hasAnyAuthority("CONSORTIUM_ADMIN", "SYSTEM_ADMIN", "ORG_ADMIN", "ORG_DEVELOPER")
                .anyRequest().authenticated().and().apply(new JwtTokenFilterConfigurer(jwtTokenProvider))
                .and().exceptionHandling().authenticationEntryPoint(restAuthticationEntryPoint)
                .and().anonymous().and().httpBasic();
    }

    @Override
    public void configure(WebSecurity web) throws Exception {
        // Allow swagger to be accessed without authentication
        web.ignoring().antMatchers("/api/agreements/1")
                .antMatchers("/api/auth/token")
                .antMatchers("/api/auth/login")
                .antMatchers("/swagger/**")
                .antMatchers("/assets/**");
    }

    @Override
    protected void configure( AuthenticationManagerBuilder auth ) throws Exception {
        auth
                .userDetailsService( userDetails() )
                .passwordEncoder( passwordEncoder() );
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
    public MyUserDetails userDetails() {
        return new MyUserDetails();
    }

}
