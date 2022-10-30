package com.shop.tbms.dto.account;

import com.shop.tbms.enumerate.Role;
import lombok.*;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

@Getter @Setter
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class CreateAccountReqDTO {
    @NotBlank
    private String username;
    @NotBlank
    private String password;
    @NotBlank
    private String fullname;
    @NotNull
    private Role role;
    private String positionCode;
}
