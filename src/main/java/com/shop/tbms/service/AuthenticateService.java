package com.shop.tbms.service;

import com.shop.tbms.dto.authen.LoginReqDTO;
import com.shop.tbms.dto.authen.LoginResDTO;
import com.shop.tbms.dto.authen.RefreshTokenReqDTO;
import org.springframework.http.HttpHeaders;

public interface AuthenticateService {
    LoginResDTO login(LoginReqDTO loginReqDTO);
    LoginResDTO refreshToken(RefreshTokenReqDTO refreshTokenReqDTO);
}
