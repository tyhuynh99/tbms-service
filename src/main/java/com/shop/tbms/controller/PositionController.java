package com.shop.tbms.controller;

import com.shop.tbms.dto.ListWrapperDTO;
import com.shop.tbms.dto.PositionDTO;
import com.shop.tbms.service.PositionService;
import com.shop.tbms.util.ResponseUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/position")
public class PositionController {
    @Autowired
    private PositionService positionService;

    @GetMapping
    public ResponseEntity<ListWrapperDTO<PositionDTO>> getAllPosition() {
        return ResponseEntity.ok(ResponseUtil.wrapListToResp(positionService.getListAllPosition()));
    }
}
