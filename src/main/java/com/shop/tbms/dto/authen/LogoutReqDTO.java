package com.shop.tbms.dto.authen;

import lombok.*;

import javax.validation.constraints.NotBlank;

@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class LogoutReqDTO {
    @NotBlank(message = "Device token is required")
    private String deviceToken;
}
