package com.shop.tbms.dto.step.report;

import lombok.*;

import javax.validation.constraints.NotNull;

@Getter
@Setter
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class ReportProgressReqDTO {
    @NotNull
    private Long progressId;

    private Boolean isCompleted;
}
