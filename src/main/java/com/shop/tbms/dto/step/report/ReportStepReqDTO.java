package com.shop.tbms.dto.step.report;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
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
@JsonIgnoreProperties(ignoreUnknown = true)
public class ReportStepReqDTO {
    @NotNull
    private Long stepId;

    private String note;

    private String factoryName;

    private LocalDateTime deliveredDate;

    private LocalDateTime receivedDate;

    private LocalDate exportDate;

    private LocalDate expectedPaidDate;

    private Boolean isPaid;

    private List<ReportProgressReqDTO> progress;

    private List<ReportChecklistReqDTO> checklist;

    private ReportEvidenceReqDTO evidence;

}
