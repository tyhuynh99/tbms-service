package com.shop.tbms.dto.request;

import lombok.*;

@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class LoginReqDTO {
    private String username;
    private String password;
}
