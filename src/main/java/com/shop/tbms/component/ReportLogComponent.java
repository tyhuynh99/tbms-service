package com.shop.tbms.component;

import com.shop.tbms.dto.step.report.ReportStepReqDTO;
import com.shop.tbms.dto.step.report_issue.ReportIssueStepReqDTO;
import com.shop.tbms.entity.ReportLog;
import com.shop.tbms.entity.Step;
import com.shop.tbms.enumerate.ReportActionType;
import com.shop.tbms.repository.ReportLogRepository;
import com.shop.tbms.util.ReportLogUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ReportLogComponent {
    @Autowired
    private ReportLogRepository reportLogRepository;

    public void insertReportLog(Step step, ReportStepReqDTO reportStepReqDTO) {
        ReportLog reportLog = new ReportLog();
        reportLog.setStep(step);
        reportLog.setAction(ReportActionType.REPORT);
        reportLog.setDescription(ReportLogUtil.generateDescription(reportStepReqDTO));

        reportLogRepository.save(reportLog);
    }

    public void insertReportLog(Step step, ReportIssueStepReqDTO reportIssueStepReqDTO) {
        ReportLog reportLog = new ReportLog();
        reportLog.setStep(step);
        reportLog.setAction(ReportActionType.REPORT_ISSUE);
        reportLog.setDescription(ReportLogUtil.generateDescription(reportIssueStepReqDTO));

        reportLogRepository.save(reportLog);
    }

}
