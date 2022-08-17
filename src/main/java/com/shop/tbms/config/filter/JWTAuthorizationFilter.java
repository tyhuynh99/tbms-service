package com.shop.tbms.config.filter;

import com.shop.tbms.config.security.AuthorizationConfig;
import com.shop.tbms.config.security.TbmsUserDetails;
import com.shop.tbms.util.AuthenticationUtil;
import com.shop.tbms.util.JWTUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Objects;

@Slf4j
@Component
public class JWTAuthorizationFilter extends OncePerRequestFilter {
    public JWTAuthorizationFilter() {
        super();
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        log.info("Process JWTAuthorizationFilter");
        log.info("Process check request {}", request.getRequestURI());

        if (Objects.nonNull(AuthenticationUtil.getUserDetails())) {
            // check role permission to request this endpoint
            TbmsUserDetails userDetails = AuthenticationUtil.getUserDetails();

            if (!AuthorizationConfig.isAllowed(request.getRequestURI(), userDetails.getRole())) {
                JWTUtil.setForbiddenResponse(response);
                log.info("End process JWTAuthorizationFilter");
                return;
            }
        }

        log.info("End process JWTAuthorizationFilter");
        filterChain.doFilter(request, response);

    }
}
