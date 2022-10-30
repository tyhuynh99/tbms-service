package com.shop.tbms.dto.account.error;

import lombok.*;

@Getter @Setter
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class CreateAccountErrorDTO {
    private String usernameError;
    private String passwordError;
    private String fullnameError;
    private String roleError;
    private String positionError;
}
