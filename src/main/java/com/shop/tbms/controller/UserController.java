package com.shop.tbms.controller;

import com.shop.tbms.annotation.ValidRole;
import com.shop.tbms.config.security.TbmsUserDetails;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.account.CreateAccountReqDTO;
import com.shop.tbms.dto.account.UpdateProfileReqDTO;
import com.shop.tbms.dto.authen.LoginReqDTO;
import com.shop.tbms.dto.authen.LoginResDTO;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.service.AccountService;
import com.shop.tbms.service.UserService;
import com.shop.tbms.util.AuthenticationUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.Map;

@RestController
@RequestMapping("/user")
public class UserController {
    @Autowired
    private UserService userService;
    @Autowired
    private AccountService accountService;

    @GetMapping("/profile")
    public ResponseEntity<Map> getProfile() {
        return ResponseEntity.ok(Map.of("profile", userService.getCurrentUserProfile()));
    }

    @PostMapping("/create")
    @ValidRole(role = {Role.PRESIDENT, Role.ACCOUNTANT})
    public ResponseEntity<SuccessRespDTO> createAccount(@RequestBody @Valid CreateAccountReqDTO createAccountReqDTO) {
        return ResponseEntity.ok(accountService.createAccount(createAccountReqDTO));
    }

    @PostMapping("/update_profile")
    public ResponseEntity<LoginResDTO> updateProfile(@RequestBody UpdateProfileReqDTO updateProfileReqDTO) {
        return ResponseEntity.ok(accountService.updateProfile(updateProfileReqDTO));
    }
}
