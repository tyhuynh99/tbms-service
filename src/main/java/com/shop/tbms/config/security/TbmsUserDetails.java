package com.shop.tbms.config.security;

import com.shop.tbms.enumerate.Role;
import lombok.*;

import java.util.List;

@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TbmsUserDetails {
    private Long userId;
    private String username;
    private String fullname;
    private Role role;
    private Boolean isActive;
}
