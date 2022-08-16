package com.shop.tbms.service.impl;

import com.shop.tbms.component.AuthenticateComponent;
import com.shop.tbms.config.security.TbmsUserDetails;
import com.shop.tbms.constant.AuthenticateConstant;
import com.shop.tbms.dto.authen.LoginReqDTO;
import com.shop.tbms.dto.authen.LoginResDTO;
import com.shop.tbms.dto.authen.RefreshTokenReqDTO;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.service.AuthenticateService;
import com.shop.tbms.util.JWTUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;

@Slf4j
@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
public class AuthenticateServiceImpl implements AuthenticateService {
    @Autowired
    private AuthenticateConstant authenticateConstant;
    @Autowired
    private AuthenticateComponent authenticateComponent;

    @Override
    public LoginResDTO login(LoginReqDTO loginReqDTO) {
        if (!StringUtils.isBlank(loginReqDTO.getUsername()) && !StringUtils.isBlank(loginReqDTO.getPassword())) {
            TbmsUserDetails userDetails = TbmsUserDetails.builder()
                    .userId(1L)
                    .username(loginReqDTO.getUsername())
                    .role(Role.ADMIN)
                    .fullname("test fullname")
                    .isActive(true)
                    .build();

            String accessToken = authenticateComponent.generateToken(true, userDetails);
            String refreshToken = authenticateComponent.generateToken(false, userDetails);

            return LoginResDTO.builder()
                    .token(accessToken)
                    .refreshToken(refreshToken)
                    .build();
        }
        return null;
    }

    @Override
    public LoginResDTO refreshToken(RefreshTokenReqDTO refreshTokenReqDTO) {
        TbmsUserDetails curRefreshTokenUserDetail = JWTUtil.getUserFromToken(refreshTokenReqDTO.getRefreshToken(), authenticateConstant.getKey());

        // TODO: query db to check current data
        String newAccessToken = authenticateComponent.generateToken(true, curRefreshTokenUserDetail);
        String newRefreshToken = authenticateComponent.generateToken(false, curRefreshTokenUserDetail);

        return LoginResDTO.builder()
                .token(newAccessToken)
                .refreshToken(newRefreshToken)
                .build();
    }
}
