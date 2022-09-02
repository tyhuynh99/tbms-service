package com.shop.tbms.controller;

import com.shop.tbms.annotation.ValidRole;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.step.detail.StepDTO;
import com.shop.tbms.dto.step.report.ReportStepReqDTO;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.service.StepService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@RestController
@RequestMapping("/step")
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
}
