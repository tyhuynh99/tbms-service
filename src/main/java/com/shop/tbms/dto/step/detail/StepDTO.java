package com.shop.tbms.dto.step.detail;

import com.shop.tbms.dto.ChecklistDTO;
import com.shop.tbms.dto.EvidenceDTO;
import com.shop.tbms.dto.step.detail.progress.MoldDeliverProgressDTO;
import com.shop.tbms.dto.step.detail.progress.MoldElementProgressDTO;
import com.shop.tbms.dto.step.detail.progress.MoldProgressDTO;
import com.shop.tbms.enumerate.step.StepStatus;
import com.shop.tbms.enumerate.step.StepType;
import lombok.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StepDTO {
    private Long id;
    private String code;
    private StepType type;
    private String name;
    private Integer sequenceNo;
    private Boolean resettable;
    private Boolean requiredEvidence;
    private Boolean isStart;
    private Boolean isEnd;
    private LocalDate expectedCompleteDate;
    private StepStatus status;
    private LocalDateTime deliveredDate;
    private LocalDateTime receivedDate;
    private String note;
    private Boolean isPaid;

    private List<ChecklistDTO> checklist;
    private List<EvidenceDTO> listEvidence;

    /* progress */
    private List<MoldProgressDTO> listMoldProgress;
    private List<MoldElementProgressDTO> listMoldElementProgress;
    private List<MoldDeliverProgressDTO> listMoldDeliverProgress;
}
