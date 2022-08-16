package com.shop.tbms.constant;

import lombok.Getter;
import lombok.Setter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Component;

@Getter
@Setter
@Component
@PropertySource("classpath:security.properties")
public class AuthenticateConstant {
    public static final String HEADER_AUTHORIZATION = "Authorization";
    public static final String TOKEN_PREFIX = "Bearer ";
    public static final String USER_ID = "userId";
    public static final String USERNAME = "username";
    public static final String FULLNAME = "fullname";
    public static final String IS_ACTIVE = "isActive";
    public static final String ROLE = "role";


    @Value("${key}")
    private String key;

    @Value("${issuer}")
    private String issuer;

    @Value("${type}")
    private String type;

    @Value("${access_token_expired_time}")
    private Long accessTokenExpiredTime;

    @Value("${refresh_token_expired_time}")
    private Long refreshTokenExpiredTime;
}
