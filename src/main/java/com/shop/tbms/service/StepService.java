package com.shop.tbms.service;

import com.shop.tbms.dto.mold.MoldDTO;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.step.ResetMoldStepReqDTO;
import com.shop.tbms.dto.step.detail.StepDTO;
import com.shop.tbms.dto.step.report.ReportStepReqDTO;
import com.shop.tbms.dto.step.report_issue.ReportIssueStepReqDTO;
import com.shop.tbms.dto.step.report_issue.ReportIssueToStepRespDTO;

import java.util.List;

public interface StepService {
    StepDTO getStep(Long stepId);
    SuccessRespDTO reportStepProgress(ReportStepReqDTO reportStepReqDTO);
    List<ReportIssueToStepRespDTO> getListToStepInReportError(Long stepId);
    SuccessRespDTO reportIssueStep(ReportIssueStepReqDTO reportIssueStepReqDTO);
    SuccessRespDTO resetMold(ResetMoldStepReqDTO resetMoldStepReqDTO);
    List<MoldDTO> getListCompletedMoldInPreviousStep(Long stepId);
}
