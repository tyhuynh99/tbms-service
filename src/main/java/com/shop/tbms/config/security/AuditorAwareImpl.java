package com.shop.tbms.config.security;

import com.shop.tbms.constant.AuthenticateConstant;
import com.shop.tbms.util.AuthenticationUtil;
import org.springframework.data.domain.AuditorAware;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
public class AuditorAwareImpl implements AuditorAware<String> {
    @Override
    public Optional<String> getCurrentAuditor() {
        return Optional
                .of(Optional.ofNullable(AuthenticationUtil.getUserDetails())
                    .map(TbmsUserDetails::getUsername)
                    .orElse(AuthenticateConstant.SYSTEM));
    }
}
