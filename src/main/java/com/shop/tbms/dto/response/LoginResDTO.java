package com.shop.tbms.dto.response;

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
    private TbmsUserDetails user;
}
