package com.shop.tbms.component;

import com.shop.tbms.entity.ReportLog;
import com.shop.tbms.entity.Step;
import com.shop.tbms.repository.ReportLogRepository;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class ReportLogComponent {
    @Autowired
    private ReportLogRepository reportLogRepository;

    public void insertReportLog(
            Step step,
            ReportLog reportLog,
            List<String> logDetail) {
        reportLog.setStep(step);
        reportLog.setDescription(StringUtils.join(logDetail, StringUtils.LF));

        reportLogRepository.save(reportLog);
    }

}
