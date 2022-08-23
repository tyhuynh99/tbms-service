package com.shop.tbms.controller;

import com.shop.tbms.annotation.ValidRole;
import com.shop.tbms.dto.PageResponse;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.order.OrderCreateReqDTO;
import com.shop.tbms.dto.order.OrderDetailRespDTO;
import com.shop.tbms.dto.order.OrderListRespDTO;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.service.PurchaseOrderService;
import com.shop.tbms.util.ResponseUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

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

    @GetMapping("/detail")
    public ResponseEntity<OrderDetailRespDTO> getDetailOrder(@RequestParam(name = "id") Long orderId) {
        return ResponseEntity.ok(purchaseOrderService.getOrderById(orderId));
    }

    @GetMapping("/list")
    public ResponseEntity<PageResponse<OrderListRespDTO>> getDetailOrder(Pageable pageable) {
        return ResponseUtil.buildPageResponse(purchaseOrderService.getListOrder(pageable));
    }
}
