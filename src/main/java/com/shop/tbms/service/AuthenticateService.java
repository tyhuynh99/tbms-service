package com.shop.tbms.service;

import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.authen.LoginReqDTO;
import com.shop.tbms.dto.authen.LoginResDTO;
import com.shop.tbms.dto.authen.LogoutReqDTO;
import com.shop.tbms.dto.authen.RefreshTokenReqDTO;
import com.shop.tbms.enumerate.Role;

import java.util.List;

public interface AuthenticateService {
    LoginResDTO login(LoginReqDTO loginReqDTO);
    LoginResDTO refreshToken(RefreshTokenReqDTO refreshTokenReqDTO);
    SuccessRespDTO logout(LogoutReqDTO logoutReqDTO);
    void checkCurrentUserRole(List<Role> listValidRole);
}
