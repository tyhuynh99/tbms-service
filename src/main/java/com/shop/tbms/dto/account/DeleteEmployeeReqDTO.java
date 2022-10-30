package com.shop.tbms.dto.account;

import lombok.*;

import javax.validation.constraints.NotBlank;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DeleteEmployeeReqDTO {
    @NotBlank
    private String username;
}
