package com.shop.tbms.service;

import com.shop.tbms.dto.request.LoginReqDTO;
import com.shop.tbms.dto.response.LoginResDTO;

public interface LoginService {
    LoginResDTO login(LoginReqDTO loginReqDTO);
}
