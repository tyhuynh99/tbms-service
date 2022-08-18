package com.shop.tbms.controller;

import com.shop.tbms.annotation.ValidRole;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.util.AuthenticationUtil;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

@RestController
@RequestMapping("/test")
public class TestController {

    @GetMapping("/profile")
    public ResponseEntity<Map> getProfile() {
        return ResponseEntity.ok(Map.of("profile", AuthenticationUtil.getUserDetails()));
    }

    @GetMapping("/admin")
    @ValidRole(role = {Role.ADMIN})
    public ResponseEntity<String> checkAdmin() {
        return ResponseEntity.ok("Admin ok");
    }

    @GetMapping("/member")
    @ValidRole(role = {Role.ADMIN, Role.MEMBER})
    public ResponseEntity<String> checkMember() {
        return ResponseEntity.ok("Member ok");
    }
}
