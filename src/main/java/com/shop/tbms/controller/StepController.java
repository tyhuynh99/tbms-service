package com.shop.tbms.controller;

import com.shop.tbms.annotation.ValidRole;
import com.shop.tbms.dto.MoldDTO;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.step.ResetMoldStepReqDTO;
import com.shop.tbms.dto.step.detail.StepDTO;
import com.shop.tbms.dto.step.report.ReportStepReqDTO;
import com.shop.tbms.dto.step.report_issue.ReportIssueStepReqDTO;
import com.shop.tbms.dto.step.report_issue.ReportIssueToStepRespDTO;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.service.StepService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

@RestController
@RequestMapping("/step")
@Validated
public class StepController {
    @Autowired
    private StepService stepService;

    @GetMapping("/detail")
    public ResponseEntity<StepDTO> getStep(@RequestParam(name = "stepId") Long stepId) {
        return ResponseEntity.ok(stepService.getStep(stepId));
    }

    @PostMapping("/report")
    @ValidRole(role = {Role.EMPLOYEE})
    public ResponseEntity<SuccessRespDTO> reportStep(@RequestBody @Valid ReportStepReqDTO reportStepReqDTO) {
        return ResponseEntity.ok(stepService.reportStepProgress(reportStepReqDTO));
    }

    @PostMapping("/report_issue")
    @ValidRole(role = {Role.EMPLOYEE})
    public ResponseEntity<SuccessRespDTO> reportIssue(@RequestBody @Valid ReportIssueStepReqDTO reportIssueStepReqDTO) {
        return ResponseEntity.ok(stepService.reportIssueStep(reportIssueStepReqDTO));
    }

    @PostMapping("/reset")
    @ValidRole(role = {Role.EMPLOYEE})
    public ResponseEntity<SuccessRespDTO> reset(@RequestBody @Valid ResetMoldStepReqDTO resetMoldStepReqDTO) {
        return ResponseEntity.ok(stepService.resetMold(resetMoldStepReqDTO));
    }

    @GetMapping("/report_issue/list_to_step")
    public ResponseEntity<List<ReportIssueToStepRespDTO>> getListToStepInReportError(@RequestParam(name = "stepId") Long stepId) {
        return ResponseEntity.ok(stepService.getListToStepInReportError(stepId));
    }

    @GetMapping("/report/completed_mold_in_pre_step")
    public ResponseEntity<List<MoldDTO>> getListCompletedMoldInPreviousStep(@RequestParam(name = "stepId") Long stepId) {
        return ResponseEntity.ok(stepService.getListCompletedMoldInPreviousStep(stepId));
    }
}
