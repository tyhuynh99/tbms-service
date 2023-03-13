package com.shop.tbms.config.security;

public class AuthorizationConfig {
    public static final String[] IGNORE_AUTHEN = {
            "/authen/login",
            "/authen/refresh",
            "/scheduler/check_late_order",
            "/file/pdf*"
    };
}
