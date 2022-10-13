package com.shop.tbms.dto.account;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class EmployeeInListDTO {
    private Long id;
    private String username;
    private String fullname;
    private String position;
}
