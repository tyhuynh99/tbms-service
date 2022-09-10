package com.shop.tbms.dto.step.report;

import lombok.*;

import javax.validation.constraints.NotNull;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class ReportStepReqDTO {
    @NotNull
    private Long stepId;

    private String note;

    private LocalDateTime deliveredDate;

    private LocalDateTime receivedDate;

    private LocalDate exportDate;

    private LocalDate expectedPaidDate;

    private Boolean isPaid;

    private List<ReportMoldProgressReqDTO> moldProgress;

    private List<ReportChecklistReqDTO> checklist;

    private List<ReportEvidenceReqDTO> evidence;

    private List<ReportMoldElementReqDTO> moldElement;
}
