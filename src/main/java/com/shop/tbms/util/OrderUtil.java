package com.shop.tbms.util;

import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.entity.PurchaseOrder;

public class OrderUtil {
    public static void validateDeletedOrder(PurchaseOrder order) {
        if (Boolean.TRUE.equals(order.getIsDeleted())) {
            throw new BusinessException("Order has been deleted.");
        }
    }
}
