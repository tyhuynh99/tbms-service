package com.shop.tbms.util;

import com.shop.tbms.dto.step.report.*;
import com.shop.tbms.dto.step.report_issue.ReportIssueStepReqDTO;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class ReportLogUtil {
    private static final String DELIMETER = ", ";

    public static String generateDescription(ReportStepReqDTO reportStepReqDTO) {
        return genMoldProgress(reportStepReqDTO.getProgress())
                + StringUtils.LF
                + genChecklist(reportStepReqDTO.getChecklist())
                + StringUtils.LF
                + genEvidence(reportStepReqDTO.getEvidence());
    }

    private static String genMoldProgress(List<ReportProgressReqDTO> moldProgressReqDTOList) {
        if (CollectionUtils.isEmpty(moldProgressReqDTOList)) return StringUtils.EMPTY;

        return "moldProgress: "
                + String.join(
                    DELIMETER,
                    moldProgressReqDTOList.stream()
                            .map(ReportProgressReqDTO::toString)
                            .collect(Collectors.toList())
                );
    }

    private static String genChecklist(List<ReportChecklistReqDTO> checklist) {
        if (CollectionUtils.isEmpty(checklist)) return StringUtils.EMPTY;

        return "checklist: "
                + String.join(
                DELIMETER,
                checklist.stream()
                        .map(ReportChecklistReqDTO::toString)
                        .collect(Collectors.toList())
        );
    }

    private static String genEvidence(ReportEvidenceReqDTO evidence) {
        if (Objects.isNull(evidence)) return StringUtils.EMPTY;

        return "evidence: "
                + String.join(
                DELIMETER,
                evidence.toString()
        );
    }
}
