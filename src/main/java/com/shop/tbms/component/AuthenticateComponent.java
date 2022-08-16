package com.shop.tbms.component;

import com.shop.tbms.config.security.TbmsUserDetails;
import com.shop.tbms.constant.AuthenticateConstant;
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
                .claim(IS_ACTIVE, userDetails.getIsActive())
                .compact();
    }
}
