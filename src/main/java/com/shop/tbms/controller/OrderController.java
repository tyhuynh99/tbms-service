package com.shop.tbms.controller;

import com.shop.tbms.annotation.ValidRole;
import com.shop.tbms.dto.PageResponse;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.order.OrderCreateReqDTO;
import com.shop.tbms.dto.order.OrderDetailRespDTO;
import com.shop.tbms.dto.order.OrderFilterReqDTO;
import com.shop.tbms.dto.order.OrderListRespDTO;
import com.shop.tbms.dto.step.UpdateExpectedCompleteReqDTO;
import com.shop.tbms.dto.step.UpdateExpectedCompleteRespDTO;
import com.shop.tbms.entity.Procedure_;
import com.shop.tbms.entity.PurchaseOrder_;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.service.PurchaseOrderService;
import com.shop.tbms.util.ResponseUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.SortDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

@RestController
@RequestMapping("/order")
@Validated
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
    public ResponseEntity<PageResponse<OrderListRespDTO>> getDetailOrder(
            OrderFilterReqDTO filterReqDTO,
            @SortDefault.SortDefaults({
                    @SortDefault(
                            sort = {
                                    PurchaseOrder_.IS_LATE,
                                    PurchaseOrder_.IS_URGENT
                            },
                            direction = Sort.Direction.DESC),
                    @SortDefault(
                            sort = {
                                    PurchaseOrder_.STATUS,
                                    PurchaseOrder_.PROCEDURE + "." + Procedure_.PRIORITY
                            },
                            direction = Sort.Direction.ASC)
            })

            Pageable pageable) {
        return ResponseUtil.buildPageResponse(purchaseOrderService.getListOrder(filterReqDTO, pageable));
    }

    @PostMapping("/step/update_expected_complete")
    @ValidRole(role = {Role.PRESIDENT, Role.SECRETARY})
    public ResponseEntity<List<UpdateExpectedCompleteRespDTO>> updateStepExceptedComplete(@RequestBody List<@Valid UpdateExpectedCompleteReqDTO> listReqDTO) {
        return ResponseEntity.ok(purchaseOrderService.updateStepExpectedComplete(listReqDTO));
    }
}