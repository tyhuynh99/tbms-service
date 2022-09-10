package com.shop.tbms.dto.step.report_error;

import lombok.*;

import javax.validation.constraints.NotNull;
import java.util.List;

@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ReportErrorStepReqDTO {
    @NotNull
    private Long stepId;

    private String note;

    @NotNull
    private List<Long> listMoldId;

    @NotNull
    private Boolean isNeedSupport;

    private Long changeToStep;
}
