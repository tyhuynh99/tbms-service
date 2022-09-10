package com.shop.tbms.service;

import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.step.detail.StepDTO;
import com.shop.tbms.dto.step.report.ReportStepReqDTO;
import com.shop.tbms.dto.step.report_error.ReportErrorToStepRespDTO;

import java.util.List;

public interface StepService {
    StepDTO getStep(Long stepId);
    SuccessRespDTO reportStepProgress(ReportStepReqDTO reportStepReqDTO);
    List<ReportErrorToStepRespDTO> getListToStepInReportError(Long stepId);
}
