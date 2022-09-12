package com.shop.tbms.dto;

import lombok.*;

@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UserAssignedStepDTO {
    private String stepCode;
    private String stepName;
}
