package com.shop.tbms.config.security;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

@Configuration
@EnableWebSecurity
public class TbmsSecurityConfig extends WebSecurityConfigurerAdapter {
    @Autowired
    private JWTAuthenticateFilter jwtAuthenticateFilter;

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http.cors()
                .and()
                .csrf().disable()
                .addFilterBefore(jwtAuthenticateFilter, UsernamePasswordAuthenticationFilter.class)
                .authorizeRequests()
                .antMatchers(HttpMethod.POST, "/authen/login").permitAll()
                .antMatchers(HttpMethod.POST, "/authen/refresh").permitAll()
                .anyRequest().authenticated()
                .and()
                .sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS)
                .and()
                .headers().httpStrictTransportSecurity()
                .maxAgeInSeconds(31536000);
    }
}
