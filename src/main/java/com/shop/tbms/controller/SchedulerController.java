package com.shop.tbms.controller;

import com.shop.tbms.service.MonitorSchedulerService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/scheduler")
public class SchedulerController {
    @Autowired
    private MonitorSchedulerService monitorSchedulerService;

    @GetMapping("/check_late_order")
    public ResponseEntity<Void> checkLateOrder() {
        monitorSchedulerService.checkLateOrder();
        return ResponseEntity.ok().build();
    }
}
