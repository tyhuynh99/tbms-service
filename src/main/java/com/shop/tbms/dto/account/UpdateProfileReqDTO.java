package com.shop.tbms.dto.account;

import lombok.*;

@Getter @Setter
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class UpdateProfileReqDTO {
    private String fullname;
    private String currentPassword;
    private String newPassword;
}
