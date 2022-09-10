package com.shop.tbms.dto.step.report_issue;

import lombok.*;

@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ReportIssueToStepRespDTO {
    private Long stepId;
    private String stepCode;
    private String stepName;
}
