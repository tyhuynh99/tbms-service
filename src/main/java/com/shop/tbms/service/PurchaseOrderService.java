package com.shop.tbms.service;

import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.order.*;
import com.shop.tbms.dto.step.upd_expect_date.UpdateExpectedCompleteReqDTO;
import com.shop.tbms.dto.step.upd_expect_date.UpdateExpectedCompleteRespDTO;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface PurchaseOrderService {
    SuccessRespDTO createOrder(OrderCreateReqDTO orderCreateReqDTO);
    SuccessRespDTO updateOrder(OrderUpdateReqDTO orderUpdateReqDTO);
    OrderDetailRespDTO getOrderById(Long orderId);
    Page<OrderListRespDTO> getListOrder(OrderFilterReqDTO filterReqDTO, Pageable pageable);
    List<UpdateExpectedCompleteRespDTO> updateStepExpectedComplete(List<UpdateExpectedCompleteReqDTO> listReqDTO);
}
