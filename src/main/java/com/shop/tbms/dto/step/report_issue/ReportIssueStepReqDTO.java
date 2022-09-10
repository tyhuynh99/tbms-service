package com.shop.tbms.dto.step.report_issue;

import lombok.*;

import javax.validation.constraints.NotNull;
import java.util.List;

@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ReportIssueStepReqDTO {
    @NotNull
    private Long stepId;

    private String description;

    @NotNull
    private List<Long> listMoldId;

    @NotNull
    private Boolean isNeedSupport;

    private Long changeToStepId;
}
