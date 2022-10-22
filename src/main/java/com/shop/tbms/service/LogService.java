package com.shop.tbms.service;

import com.shop.tbms.dto.log.IssueLogDTO;
import com.shop.tbms.dto.log.ReportLogRespDTO;

import java.util.List;

public interface LogService {
    ReportLogRespDTO getReportLog(long stepId);
    List<IssueLogDTO> getIssueLog(long stepId);
}
