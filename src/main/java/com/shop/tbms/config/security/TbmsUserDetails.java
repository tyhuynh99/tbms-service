package com.shop.tbms.config.security;

import com.shop.tbms.enumerate.Role;
import lombok.*;

import java.util.Objects;

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
    private Boolean active;

    @Override
    public boolean equals(Object obj) {
        // if 2 user have the same user id and username -> equals
        if (!(obj instanceof TbmsUserDetails)) {
            return false;
        }

        TbmsUserDetails otherUserDetail = (TbmsUserDetails) obj;

        // compare user id
        if (!Objects.equals(otherUserDetail.getUserId(), this.getUserId())) return false;
        // compare username
        if (!Objects.equals(otherUserDetail.getUsername(), this.getUsername())) return false;

        return true;
    }
}
