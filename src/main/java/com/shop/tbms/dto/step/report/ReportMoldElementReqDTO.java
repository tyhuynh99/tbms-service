package com.shop.tbms.dto.step.report;

import lombok.*;

import javax.validation.constraints.NotBlank;

@Getter
@Setter
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class ReportMoldElementReqDTO {
    @NotBlank
    private String code;

    @NotBlank
    private String description;
}
