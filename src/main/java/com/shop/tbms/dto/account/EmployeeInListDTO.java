package com.shop.tbms.dto.account;

import lombok.*;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class EmployeeInListDTO {
    private Long id;
    private String username;
    private String fullname;
    private String position;
}
