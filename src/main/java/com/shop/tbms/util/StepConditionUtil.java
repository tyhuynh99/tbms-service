package com.shop.tbms.util;

import com.shop.tbms.constant.StepConstant;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.entity.Step;

import java.util.Optional;

public class StepConditionUtil {
    public static boolean canGenProgressWhenCreateOrder(Step step, StepConstant stepConstant) {
        if (Boolean.TRUE.equals(step.getHasCondition()) && stepConstant.getCodePHONG_DIEN().equals(step.getCode())) {
            return false;
        }
        return true;
    }

    public static Optional<Step> getStepPhongDien(PurchaseOrder order, StepConstant stepConstant) {
        return order.getProcedure().getListStep().stream()
                .filter(step -> stepConstant.getCodePHONG_DIEN().equalsIgnoreCase(step.getCode()))
                .findFirst();
    }
}
