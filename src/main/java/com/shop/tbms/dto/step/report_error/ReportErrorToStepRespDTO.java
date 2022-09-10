package com.shop.tbms.dto.step.report_error;

import com.shop.tbms.enumerate.StepType;
import lombok.*;

@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ReportErrorToStepRespDTO {
    private Long stepId;
    private String stepCode;
    private String stepName;
}
