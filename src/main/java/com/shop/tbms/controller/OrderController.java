package com.shop.tbms.controller;

import com.shop.tbms.annotation.ValidRole;
import com.shop.tbms.dto.FileDTO;
import com.shop.tbms.dto.ListWrapperDTO;
import com.shop.tbms.dto.PDFDto;
import com.shop.tbms.dto.PageResponse;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.order.OrderCreateReqDTO;
import com.shop.tbms.dto.order.OrderDetailRespDTO;
import com.shop.tbms.dto.order.OrderFilterReqDTO;
import com.shop.tbms.dto.order.OrderListRespDTO;
import com.shop.tbms.dto.order.OrderUpdateReqDTO;
import com.shop.tbms.dto.step.upd_expect_date.UpdateExpectedCompleteReqDTO;
import com.shop.tbms.dto.step.upd_expect_date.UpdateExpectedCompleteRespDTO;
import com.shop.tbms.entity.Procedure_;
import com.shop.tbms.entity.PurchaseOrder_;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.service.PurchaseOrderService;
import com.shop.tbms.util.ResponseUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.SortDefault;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.CollectionUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

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

    @PutMapping("/update")
    @ValidRole(role = {Role.PRESIDENT, Role.SECRETARY})
    public ResponseEntity<SuccessRespDTO> updateOrder(@RequestBody @Valid OrderUpdateReqDTO orderUpdateReqDTO) {
        return ResponseEntity.ok(purchaseOrderService.updateOrder(orderUpdateReqDTO));
    }

    @GetMapping("/detail")
    public ResponseEntity<OrderDetailRespDTO> getDetailOrder(@RequestParam(name = "id") Long orderId) {
        return ResponseEntity.ok(purchaseOrderService.getOrderById(orderId));
    }

    @DeleteMapping
    @ValidRole(role = {Role.PRESIDENT, Role.SECRETARY, Role.ACCOUNTANT})
    public ResponseEntity<SuccessRespDTO> deleteOrder(@RequestParam(name = "id") Long orderId) {
        return ResponseEntity.ok(purchaseOrderService.deleteOrder(orderId));
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
                            direction = Sort.Direction.ASC),
                    @SortDefault(
                            sort = {
                                    PurchaseOrder_.CREATED_DATE
                            },
                            direction = Sort.Direction.DESC)
            })
            Pageable pageable) {
        return ResponseUtil.buildPageResponse(purchaseOrderService.getListOrder(filterReqDTO, pageable));
    }

    @PostMapping("/step/update_expected_complete")
    @ValidRole(role = {Role.PRESIDENT, Role.SECRETARY})
    public ResponseEntity<UpdateExpectedCompleteRespDTO> updateStepExceptedComplete(@RequestBody @Valid UpdateExpectedCompleteReqDTO reqDTO) {
        List<UpdateExpectedCompleteRespDTO> listResp = purchaseOrderService.updateStepExpectedComplete(List.of(reqDTO));
        return ResponseEntity.ok(CollectionUtils.isEmpty(listResp) ? null : listResp.get(0));
    }

    @PostMapping(value = "/pdf", consumes = {MediaType.APPLICATION_PDF_VALUE, "multipart/form-data"})
    public ResponseEntity<List<FileDTO>> uploadPDF(@RequestParam("orderId") Long orderId, @RequestPart("files") MultipartFile[] files) throws Exception {
        List<FileDTO> result = purchaseOrderService.uploadPDF(orderId, files);
        return ResponseEntity.ok(result);
    }

    @GetMapping(value = "/pdf")
    public ResponseEntity<ListWrapperDTO<PDFDto>> getPDF(@RequestParam("orderId") Long orderId) {
        ListWrapperDTO<PDFDto> result = purchaseOrderService.getPDF(orderId);
        return ResponseEntity.ok(result);
    }

    @DeleteMapping(value = "/pdf")
    public ResponseEntity<SuccessRespDTO> deletePDF(@RequestParam("pdfId") Long pdfId) {
        SuccessRespDTO result = purchaseOrderService.deletePDF(pdfId);
        return ResponseEntity.ok(result);
    }
}
