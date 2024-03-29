package com.shop.tbms.component;

import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.config.security.TbmsUserDetails;
import com.shop.tbms.constant.AuthenticateConstant;
import com.shop.tbms.dto.authen.LoginReqDTO;
import com.shop.tbms.entity.Account;
import com.shop.tbms.util.DateTimeUtil;
import com.shop.tbms.util.PasswordUtil;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Date;

import static com.shop.tbms.constant.AuthenticateConstant.*;

@Component
public class AuthenticateComponent {
    @Autowired
    private AuthenticateConstant authenticateConstant;

    public String generateToken(boolean isAccessToken, TbmsUserDetails userDetails) {
        String signingKey = authenticateConstant.getKey();
        Long expiredTime = isAccessToken
                ? authenticateConstant.getAccessTokenExpiredTime()
                : authenticateConstant.getRefreshTokenExpiredTime();

        return Jwts.builder()
                .signWith(Keys.hmacShaKeyFor(signingKey.getBytes()), SignatureAlgorithm.HS512)
                .setIssuer(authenticateConstant.getIssuer())
                .setExpiration(new Date(System.currentTimeMillis() + expiredTime))
                .claim(USER_ID, userDetails.getUserId())
                .claim(USERNAME, userDetails.getUsername())
                .claim(FULLNAME, userDetails.getFullname())
                .claim(ROLE, userDetails.getRole())
                .claim(IS_ACTIVE, userDetails.getActive())
                .claim(UPDATED_DATE, DateTimeUtil.formatDate(userDetails.getUpdatedDate()))
                .compact();
    }

    public void checkValidAccount(Account account, LoginReqDTO loginReqDTO) {
        // check correct password
        if (!PasswordUtil.checkPassword(loginReqDTO.getPassword(), account.getPassword())) {
            throw new BusinessException("Invalid username or password");
        }

        // check active
        this.checkActiveAccount(account);
    }

    public void checkActiveAccount(Account account) {
        // check active
        if (!Boolean.TRUE.equals(account.getActive())) {
            throw new BusinessException("Account is locked");
        }
    }
}
