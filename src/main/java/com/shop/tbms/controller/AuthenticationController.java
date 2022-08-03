package com.shop.tbms.controller;

import com.shop.tbms.dto.request.LoginReqDTO;
import com.shop.tbms.dto.response.LoginResDTO;
import com.shop.tbms.service.LoginService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/authen")
public class AuthenticationController {
    @Autowired
    private LoginService loginService;

    @PostMapping("/login")
    public ResponseEntity<LoginResDTO> login(@RequestBody LoginReqDTO loginReqDTO) {
        return ResponseEntity.ok(loginService.login(loginReqDTO));
    }
}
