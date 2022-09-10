package com.shop.tbms.util;

import com.shop.tbms.dto.step.report.*;
import com.shop.tbms.dto.step.report_issue.ReportIssueStepReqDTO;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.CollectionUtils;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class ReportLogUtil {
    private static final String DELIMETER = ", ";

    public static String generateDescription(ReportStepReqDTO reportStepReqDTO) {
        return genMoldProgress(reportStepReqDTO.getMoldProgress())
                + StringUtils.LF
                + genChecklist(reportStepReqDTO.getChecklist())
                + StringUtils.LF
                + genEvidence(reportStepReqDTO.getEvidence())
                + StringUtils.LF
                + genMoldElement(reportStepReqDTO.getMoldElement());
    }

    private static String genMoldProgress(List<ReportMoldProgressReqDTO> moldProgressReqDTOList) {
        if (CollectionUtils.isEmpty(moldProgressReqDTOList)) return StringUtils.EMPTY;

        return "moldProgress: "
                + String.join(
                    DELIMETER,
                    moldProgressReqDTOList.stream()
                            .map(ReportMoldProgressReqDTO::toString)
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

    private static String genEvidence(List<ReportEvidenceReqDTO> evidence) {
        if (CollectionUtils.isEmpty(evidence)) return StringUtils.EMPTY;

        return "evidence: "
                + String.join(
                DELIMETER,
                evidence.stream()
                        .map(ReportEvidenceReqDTO::toString)
                        .collect(Collectors.toList())
        );
    }

    private static String genMoldElement(List<ReportMoldElementReqDTO> reportMoldElementReqDTOList) {
        if (CollectionUtils.isEmpty(reportMoldElementReqDTOList)) return StringUtils.EMPTY;

        return "moldElement: "
                + String.join(
                DELIMETER,
                reportMoldElementReqDTOList.stream()
                        .map(ReportMoldElementReqDTO::toString)
                        .collect(Collectors.toList())
        );
    }

    public static String generateDescription(ReportIssueStepReqDTO reportIssueStepReqDTO) {
        return reportIssueStepReqDTO.toString();
    }
}
