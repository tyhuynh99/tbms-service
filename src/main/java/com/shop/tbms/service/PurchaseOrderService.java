package com.shop.tbms.service;

import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.order.OrderCreateReqDTO;
import com.shop.tbms.dto.order.OrderDetailRespDTO;
import com.shop.tbms.dto.order.OrderFilterReqDTO;
import com.shop.tbms.dto.order.OrderListRespDTO;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface PurchaseOrderService {
    SuccessRespDTO createOrder(OrderCreateReqDTO orderCreateReqDTO);
    OrderDetailRespDTO getOrderById(Long orderId);
    Page<OrderListRespDTO> getListOrder(OrderFilterReqDTO filterReqDTO, Pageable pageable);
}
