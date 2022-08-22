package com.shop.tbms.service.impl;

import com.shop.tbms.constant.MessageConstant;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.order.OrderCreateReqDTO;
import com.shop.tbms.service.PurchaseOrderService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
public class PurchaseOrderServiceImpl implements PurchaseOrderService {
    @Autowired
    private MessageConstant messageConstant;

    @Override
    public SuccessRespDTO createOrder(OrderCreateReqDTO orderCreateReqDTO) {
        /* Generate Procedure entity with Steps */

        /* Insert data procedure & step */

        /* Generate StepSequence from template */

        /* Insert data StepSequence */

        return SuccessRespDTO.builder()
                .message(messageConstant.getCreateSuccess())
                .build();
    }
}
