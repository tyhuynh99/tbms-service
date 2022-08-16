package com.shop.tbms.dto.authen;

import com.shop.tbms.config.security.TbmsUserDetails;
import lombok.*;

@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class LoginResDTO {
    private String token;
    private String refreshToken;
    private TbmsUserDetails user;
}
