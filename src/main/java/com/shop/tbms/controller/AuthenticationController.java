package com.shop.tbms.controller;

import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.authen.LoginReqDTO;
import com.shop.tbms.dto.authen.LoginResDTO;
import com.shop.tbms.dto.authen.LogoutReqDTO;
import com.shop.tbms.dto.authen.RefreshTokenReqDTO;
import com.shop.tbms.service.AuthenticateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/authen")
public class AuthenticationController {
    @Autowired
    private AuthenticateService authenticateService;

    @PostMapping("/login")
    public ResponseEntity<LoginResDTO> login(@RequestBody @Validated LoginReqDTO loginReqDTO) {
        return ResponseEntity.ok(authenticateService.login(loginReqDTO));
    }

    @PostMapping("/logout")
    public ResponseEntity<SuccessRespDTO> logout(@RequestBody @Validated LogoutReqDTO logoutReqDTO) {
        return ResponseEntity.ok(authenticateService.logout(logoutReqDTO));
    }

    @PostMapping("/refresh")
    public ResponseEntity<LoginResDTO> refreshToken(@RequestBody @Validated RefreshTokenReqDTO refreshTokenReqDTO) {
        return ResponseEntity.ok(authenticateService.refreshToken(refreshTokenReqDTO));
    }
}
