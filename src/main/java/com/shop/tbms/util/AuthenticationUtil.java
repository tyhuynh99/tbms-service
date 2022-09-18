package com.shop.tbms.util;

import com.shop.tbms.config.security.TbmsUserDetails;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.Objects;

@Slf4j
public class AuthenticationUtil {
    public static TbmsUserDetails getUserDetails() {
        try {
            return (Objects.nonNull(SecurityContextHolder.getContext().getAuthentication())
                    &&
                    (SecurityContextHolder.getContext().getAuthentication().getPrincipal() instanceof TbmsUserDetails))
                    ?
                    (TbmsUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()
                    :
                    null;
        } catch (Exception e) {
            log.error("Error while get user detail {}", e);
        }
        return null;
    }
}
