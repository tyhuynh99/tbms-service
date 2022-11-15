package com.shop.tbms.dto;

import com.shop.tbms.config.security.TbmsUserDetails;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class ProfileDTO extends TbmsUserDetails {
    public ProfileDTO(TbmsUserDetails userDetails) {
        super(
                userDetails.getUserId(),
                userDetails.getUsername(),
                userDetails.getFullname(),
                userDetails.getRole(),
                userDetails.getActive(),
                userDetails.getPosition(),
                userDetails.getPositionCode(),
                userDetails.getUpdatedDate());
    }

    private boolean hasUnreadNoti;
}
