package com.shop.tbms.controller;

import com.shop.tbms.dto.step.detail.StepDTO;
import com.shop.tbms.service.StepService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/step")
public class StepController {
    @Autowired
    private StepService stepService;

    @GetMapping("/detail")
    public ResponseEntity<StepDTO> getStep(@RequestParam(name = "stepId") Long stepId) {
        return ResponseEntity.ok(stepService.getStep(stepId));
    }
}
