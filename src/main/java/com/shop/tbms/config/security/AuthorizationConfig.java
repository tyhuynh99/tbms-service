package com.shop.tbms.config.security;

import com.shop.tbms.enumerate.Role;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class AuthorizationConfig {
    public static final String[] IGNORE_AUTHEN = {
            "/authen/login",
            "/authen/refresh",
            "/scheduler/check_late_order",
            "/file/pdf*"
    };
}
