package com.shop.tbms.controller;

import com.shop.tbms.config.security.TbmsUserDetails;
import com.shop.tbms.service.UserService;
import com.shop.tbms.util.AuthenticationUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

@RestController
@RequestMapping("/user")
public class UserController {
    @Autowired
    private UserService userService;

    @GetMapping("/profile")
    public ResponseEntity<Map> getProfile() {
        return ResponseEntity.ok(Map.of("profile", AuthenticationUtil.getUserDetails()));
    }

    @GetMapping("/assigned_step")
    public ResponseEntity<TbmsUserDetails> getProfileWithAssignedStep() {
        return ResponseEntity.ok(userService.setUserStepToUser());
    }
}
