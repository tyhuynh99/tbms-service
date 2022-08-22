package com.shop.tbms.service;

import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.order.OrderCreateReqDTO;

public interface PurchaseOrderService {
    SuccessRespDTO createOrder(OrderCreateReqDTO orderCreateReqDTO);
}
