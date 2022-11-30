package com.shop.tbms.controller;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.shop.tbms.annotation.ValidRole;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.mold.MoldDTO;
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
    public ResponseEntity<SuccessRespDTO> reportStep(
            @RequestPart("data") String reportStepReqDTOJson,
            @RequestPart(value = "listFile", required = false) List<MultipartFile> listFile
    ) throws JsonProcessingException {
        log.info("Covert JSON {} to object", reportStepReqDTOJson);
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        ReportStepReqDTO reportStepReqDTO = objectMapper.readValue(reportStepReqDTOJson, ReportStepReqDTO.class);

        return processReportStep(reportStepReqDTO, listFile);
    }

    @PostMapping(value = "/test", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<SuccessRespDTO> test(@RequestPart("data") String data) throws JsonProcessingException {
        ObjectMapper objectMapper = new ObjectMapper();
        SuccessRespDTO dto = objectMapper.readValue(data, SuccessRespDTO.class);

        return ResponseEntity.ok(dto);
    }

    @PostMapping(value = "/report_without_file")
    public ResponseEntity reportWithoutFile(@RequestBody ReportStepReqDTO reportStepReqDTO) {
        return processReportStep(reportStepReqDTO, null);
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

    @PostMapping("/report_issue")
    public ResponseEntity<SuccessRespDTO> reportIssue(@RequestBody @Valid ReportIssueStepReqDTO reportIssueStepReqDTO) {
        return ResponseEntity.ok(stepService.reportIssueStep(reportIssueStepReqDTO));
    }

    @PostMapping("/reset")
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
