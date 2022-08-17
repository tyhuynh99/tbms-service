package com.shop.tbms.util;

import com.shop.tbms.config.security.TbmsUserDetails;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.Objects;

public class AuthenticationUtil {
    public static TbmsUserDetails getUserDetails() {
        return (Objects.nonNull(SecurityContextHolder.getContext().getAuthentication()))
                ?
                (TbmsUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()
                :
                null;
    }
}
