package com.shop.tbms.service.impl;

import com.shop.tbms.constant.AuthenticateConstant;
import com.shop.tbms.dto.request.LoginReqDTO;
import com.shop.tbms.dto.response.LoginResDTO;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.service.LoginService;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;
import lombok.extern.java.Log;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;

import static com.shop.tbms.constant.AuthenticateConstant.*;

@Slf4j
@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
public class LoginServiceImpl implements LoginService {
    @Autowired
    private AuthenticateConstant authenticateConstant;

    @Override
    public LoginResDTO login(LoginReqDTO loginReqDTO) {
        if (!StringUtils.isBlank(loginReqDTO.getUsername()) && !StringUtils.isBlank(loginReqDTO.getPassword())) {
            String signingKey = authenticateConstant.getKey();

            String token = Jwts.builder()
                    .signWith(Keys.hmacShaKeyFor(signingKey.getBytes()), SignatureAlgorithm.HS512)
                    .setIssuer(authenticateConstant.getIssuer())
                    .setExpiration(new Date(System.currentTimeMillis() + 864000000))
                    .claim(USER_ID, 1)
                    .claim(USERNAME, loginReqDTO.getUsername())
                    .claim(FULLNAME, "test")
                    .claim(ROLE, Role.ADMIN)
                    .claim(IS_ACTIVE, true)
                    .compact();

            return LoginResDTO.builder().token(token).build();
        }
        return null;
    }
}
