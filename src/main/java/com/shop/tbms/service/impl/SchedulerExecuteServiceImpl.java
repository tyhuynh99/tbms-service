package com.shop.tbms.service.impl;

import com.shop.tbms.service.PurchaseOrderService;
import com.shop.tbms.service.SchedulerExecuteService;
import com.shop.tbms.service.StepService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
public class SchedulerExecuteServiceImpl implements SchedulerExecuteService {
    @Autowired
    private PurchaseOrderService purchaseOrderService;
    @Autowired
    private StepService stepService;

    @Override
    public void exeCheckLateOrder() {
        purchaseOrderService.checkLateOrder();
        purchaseOrderService.notiNearlyDueOrder();

        stepService.notiStepLate();
        stepService.notiStepNearlyLate();
    }
}
