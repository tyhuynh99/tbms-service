package com.shop.tbms.controller;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.shop.tbms.annotation.ValidRole;
import com.shop.tbms.dto.mold.MoldDTO;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.step.ResetMoldStepReqDTO;
import com.shop.tbms.dto.step.detail.StepDTO;
import com.shop.tbms.dto.step.report.ReportEvidenceReqDTO;
import com.shop.tbms.dto.step.report.ReportStepReqDTO;
import com.shop.tbms.dto.step.report_issue.ReportIssueStepReqDTO;
import com.shop.tbms.dto.step.report_issue.ReportIssueToStepRespDTO;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.service.StepService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.Valid;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("/step")
@Validated
@Slf4j
public class StepController {
    @Autowired
    private StepService stepService;

    @GetMapping("/detail")
    public ResponseEntity<StepDTO> getStep(@RequestParam(name = "stepId") Long stepId) {
        return ResponseEntity.ok(stepService.getStep(stepId));
    }

    @PostMapping(value = "/report", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @ValidRole(role = {Role.EMPLOYEE})
    public ResponseEntity<SuccessRespDTO> reportStep(
            @RequestPart("data") String reportStepReqDTOJson,
            @RequestPart(value = "listFile", required = false) List<MultipartFile> listFile
    ) throws JsonProcessingException {
        log.info("Covert JSON {} to object", reportStepReqDTOJson);
        ObjectMapper objectMapper = new ObjectMapper();
        ReportStepReqDTO reportStepReqDTO = objectMapper.readValue(reportStepReqDTOJson, ReportStepReqDTO.class);

        return processReportStep(reportStepReqDTO, listFile);
    }

    private ResponseEntity<SuccessRespDTO> processReportStep(
            @Valid ReportStepReqDTO reportStepReqDTO,
            List<MultipartFile> listFile) {
        Optional<ReportEvidenceReqDTO> evidenceChk = Optional.ofNullable(reportStepReqDTO).map(ReportStepReqDTO::getEvidence);
        if (evidenceChk.isPresent()) {
            evidenceChk.get().setListFile(listFile);
        } else {
            reportStepReqDTO.setEvidence(
                    ReportEvidenceReqDTO.builder()
                            .listFile(listFile)
                            .build());
        }

        return ResponseEntity.ok(stepService.reportStepProgress(reportStepReqDTO));
    }

    @PostMapping(value = "/test/report", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @ValidRole(role = {Role.EMPLOYEE})
    public ResponseEntity testShowReportDTO(ReportStepReqDTO reportStepReqDTO) {
        return ResponseEntity.ok(null);
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
