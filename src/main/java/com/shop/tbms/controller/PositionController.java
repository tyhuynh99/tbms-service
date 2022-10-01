package com.shop.tbms.controller;

import com.shop.tbms.dto.PositionDTO;
import com.shop.tbms.service.PositionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/position")
public class PositionController {
    @Autowired
    private PositionService positionService;

    @GetMapping
    public ResponseEntity<List<PositionDTO>> getAllPosition() {
        return ResponseEntity.ok(positionService.getListAllPosition());
    }
}
