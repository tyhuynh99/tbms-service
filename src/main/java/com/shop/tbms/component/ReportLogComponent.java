package com.shop.tbms.component;

import com.shop.tbms.dto.step.report.ReportStepReqDTO;
import com.shop.tbms.dto.step.report_issue.ReportIssueStepReqDTO;
import com.shop.tbms.entity.ReportLog;
import com.shop.tbms.entity.Step;
import com.shop.tbms.repository.ReportLogRepository;
import com.shop.tbms.util.ReportLogUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ReportLogComponent {
    @Autowired
    private ReportLogRepository reportLogRepository;

    public void insertReportLog(Step step, ReportStepReqDTO reportStepReqDTO, ReportLog reportLog) {
        reportLog.setStep(step);
        reportLog.setDescription(ReportLogUtil.generateDescription(reportStepReqDTO));

        reportLogRepository.save(reportLog);
    }

}
