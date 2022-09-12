package com.shop.tbms.service.impl;

import com.shop.tbms.component.AuthenticateComponent;
import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.config.exception.ForbiddenException;
import com.shop.tbms.config.security.TbmsUserDetails;
import com.shop.tbms.constant.AuthenticateConstant;
import com.shop.tbms.dto.authen.LoginReqDTO;
import com.shop.tbms.dto.authen.LoginResDTO;
import com.shop.tbms.dto.authen.RefreshTokenReqDTO;
import com.shop.tbms.entity.Account;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.mapper.AccountToUserDetailsMapper;
import com.shop.tbms.repository.AccountRepository;
import com.shop.tbms.repository.TemplateStepRepository;
import com.shop.tbms.service.AuthenticateService;
import com.shop.tbms.util.AuthenticationUtil;
import com.shop.tbms.util.JWTUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Slf4j
@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
public class AuthenticateServiceImpl implements AuthenticateService {
    @Autowired
    private AuthenticateConstant authenticateConstant;

    @Autowired
    private AccountRepository accountRepository;
    @Autowired
    private TemplateStepRepository templateStepRepository;

    @Autowired
    private AuthenticateComponent authenticateComponent;

    @Autowired
    private AccountToUserDetailsMapper accountToUserDetailsMapper;


    @Override
    public LoginResDTO login(LoginReqDTO loginReqDTO) {
        Account account = accountRepository.findFirstByUsername(loginReqDTO.getUsername())
                .orElseThrow(() -> new BusinessException("Invalid username or password"));

        // check valid account
        authenticateComponent.checkValidAccount(account, loginReqDTO);

        TbmsUserDetails userDetails = accountToUserDetailsMapper.toUserDetails(account);

        String accessToken = authenticateComponent.generateToken(true, userDetails);
        String refreshToken = authenticateComponent.generateToken(false, userDetails);

        return LoginResDTO.builder()
                .token(accessToken)
                .refreshToken(refreshToken)
                .user(userDetails)
                .build();
    }

    @Override
    public LoginResDTO refreshToken(RefreshTokenReqDTO refreshTokenReqDTO) {
        TbmsUserDetails curRefreshTokenUserDetail = JWTUtil.getUserFromToken(refreshTokenReqDTO.getRefreshToken(), authenticateConstant.getKey());

        Account account = accountRepository.findFirstByUsername(curRefreshTokenUserDetail.getUsername())
                .orElseThrow(() -> new BusinessException("Invalid username or password"));

        // check account is active
        authenticateComponent.checkActiveAccount(account);

        String newAccessToken = authenticateComponent.generateToken(true, curRefreshTokenUserDetail);
        String newRefreshToken = authenticateComponent.generateToken(false, curRefreshTokenUserDetail);

        return LoginResDTO.builder()
                .token(newAccessToken)
                .refreshToken(newRefreshToken)
                .build();
    }

    @Override
    public void checkCurrentUserRole(List<Role> listValidRole) {
        TbmsUserDetails userDetails = AuthenticationUtil.getUserDetails();

        Account account = accountRepository.findFirstByUsername(userDetails.getUsername())
                .orElseThrow(() -> new BusinessException("Invalid username or password"));

        // check account is active
        authenticateComponent.checkActiveAccount(account);

        if (!listValidRole.contains(account.getRole())) {
            throw new ForbiddenException("User cannot access to this feature");
        }
    }
}
