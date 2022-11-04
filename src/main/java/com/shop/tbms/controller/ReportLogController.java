package com.shop.tbms.controller;

import com.shop.tbms.dto.ListWrapperDTO;
import com.shop.tbms.dto.log.ReportLogDetailDTO;
import com.shop.tbms.dto.log.ReportLogRespDTO;
import com.shop.tbms.service.LogService;
import com.shop.tbms.util.ResponseUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/log")
public class ReportLogController {
    @Autowired
    private LogService logService;

    @GetMapping("/report")
    public ResponseEntity<ReportLogRespDTO> getReportLog(@RequestParam("stepId") long stepId) {
        return ResponseEntity.ok(logService.getReportLog(stepId));
    }

    @GetMapping("/issue")
    public ResponseEntity<ListWrapperDTO<ReportLogDetailDTO>> getIssue(@RequestParam("stepId") long stepId) {
        return ResponseEntity.ok(ResponseUtil.wrapListToResp(logService.getIssueLog(stepId)));
    }
}
