package com.shop.tbms.config.filter;

import com.shop.tbms.config.exception.ForbiddenException;
import com.shop.tbms.config.security.TbmsUserDetails;
import com.shop.tbms.constant.AuthenticateConstant;
import com.shop.tbms.entity.Account;
import com.shop.tbms.repository.AccountRepository;
import com.shop.tbms.util.DateTimeUtil;
import com.shop.tbms.util.JWTUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import javax.persistence.EntityNotFoundException;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Objects;

@Slf4j
@Component
public class JWTAuthenticateFilter extends OncePerRequestFilter {
    @Autowired
    private AuthenticateConstant authenticateConstant;

    @Autowired
    private AccountRepository accountRepository;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        log.info("Process JWTAuthenticateFilter");
        String jwt = JWTUtil.parseJwt(request);

        if (!StringUtils.isBlank(jwt)) {
            // verify token
            TbmsUserDetails userDetails = JWTUtil.getUserFromToken(jwt, authenticateConstant.getKey());

            if (Objects.nonNull(userDetails)) {
                // check profile updated date, if different -> profile has updated
                Account account = accountRepository.findFirstByUsername(userDetails.getUsername()).orElseThrow(EntityNotFoundException::new);
                if (Objects.isNull(userDetails.getUpdatedDate()) || !DateTimeUtil.isRelativeEqual(userDetails.getUpdatedDate(), account.getUpdatedDate())) {
                    log.info("Account {} is updated. Forbidden", account);
                    throw new ForbiddenException("Account is updated");
                }

                if (Objects.nonNull(account.getPosition())) {
                    userDetails.setPositionCode(account.getPosition().getCode());
                    userDetails.setPosition(account.getPosition().getName());
                }

                UsernamePasswordAuthenticationToken authenticationToken = new UsernamePasswordAuthenticationToken(userDetails, null, null);
                SecurityContextHolder.getContext().setAuthentication(authenticationToken);

                log.info("End process JWTAuthenticateFilter");
                filterChain.doFilter(request, response);
                return;
            }
        }
        log.info("End process JWTAuthenticateFilter");
        filterChain.doFilter(request, response);
    }
}
