package com.shop.tbms.service;

import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.step.detail.StepDTO;
import com.shop.tbms.dto.step.report.ReportStepReqDTO;

public interface StepService {
    StepDTO getStep(Long stepId);
    SuccessRespDTO reportStepProgress(ReportStepReqDTO reportStepReqDTO);
}
