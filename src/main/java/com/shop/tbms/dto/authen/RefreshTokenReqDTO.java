package com.shop.tbms.dto.authen;

import lombok.*;

import javax.validation.constraints.NotBlank;

@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class RefreshTokenReqDTO {
    @NotBlank(message = "Refresh token is required")
    private String refreshToken;
}
