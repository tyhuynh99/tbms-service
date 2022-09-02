package com.shop.tbms.dto.step.report;

import lombok.*;

import javax.validation.constraints.NotNull;

@Getter
@Setter
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class ReportChecklistReqDTO {
    @NotNull
    private Long checklistId;

    private Boolean isChecked;
}
