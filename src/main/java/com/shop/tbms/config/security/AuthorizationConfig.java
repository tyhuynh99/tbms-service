package com.shop.tbms.config.security;

import com.shop.tbms.enumerate.Role;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class AuthorizationConfig {
    public static final String LOGIN = "/authen/login";
    public static final String REFRESH_TOKEN = "/authen/refresh";
    public static final String CHECK_ADMIN = "/test/admin";
    public static final String CHECK_MEMBER = "/test/member";
    public static final String GET_PROFILE = "/test/profile";

    public static List<String> getUnauthorizedAPI() {
        return Arrays.asList(
                LOGIN,
                REFRESH_TOKEN
        );
    }

    public static List<String> getAdminAPI() {
        return Arrays.asList(CHECK_ADMIN, CHECK_MEMBER, GET_PROFILE);
    }

    public static List<String> getMemberAPI() {
        return Arrays.asList(CHECK_MEMBER, GET_PROFILE);
    }

    public static boolean isAllowedForUnauthorizedAPI(String uri) {
        return getUnauthorizedAPI().stream().anyMatch(uri::contains);
    }

    public static boolean isAllowed(String uri, Role role) {
        List<String> listAllowedAPI;

        switch (role) {
            case MEMBER:
                listAllowedAPI = getMemberAPI();
                break;
            case ADMIN:
                listAllowedAPI = getAdminAPI();
                break;
            default:
                listAllowedAPI = new ArrayList<>();
        }

        return listAllowedAPI.stream().anyMatch(uri::contains);
    }
}
