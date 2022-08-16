package com.shop.tbms.util;

import com.shop.tbms.config.security.TbmsUserDetails;
import org.springframework.security.core.context.SecurityContextHolder;

public class AuthenticationUtil {
    public static TbmsUserDetails getUserDetails() {
        return  (TbmsUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
    }
}
