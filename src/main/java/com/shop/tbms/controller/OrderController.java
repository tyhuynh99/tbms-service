package com.shop.tbms.controller;

import com.shop.tbms.annotation.ValidRole;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.order.OrderCreateReqDTO;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.service.PurchaseOrderService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
@RequestMapping("/order")
public class OrderController {
    @Autowired
    private PurchaseOrderService purchaseOrderService;

    @PostMapping
    @ValidRole(role = {Role.PRESIDENT, Role.SECRETARY})
    public ResponseEntity<SuccessRespDTO> createOrder(@RequestBody @Valid OrderCreateReqDTO orderCreateReqDTO) {
        return ResponseEntity.ok(purchaseOrderService.createOrder(orderCreateReqDTO));
    }
}
