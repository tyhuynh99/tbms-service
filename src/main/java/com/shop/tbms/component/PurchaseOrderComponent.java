package com.shop.tbms.component;

import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.enumerate.order.OrderStatus;
import com.shop.tbms.util.AuthenticationUtil;
import com.shop.tbms.util.OrderUtil;
import org.springframework.stereotype.Component;

@Component
public class PurchaseOrderComponent {
    public void canUpdateOrder(PurchaseOrder currentOrder) {
        /* validate delete order */
        OrderUtil.validateDeletedOrder(currentOrder);

        /* check order status */
        Role curRole = AuthenticationUtil.getUserDetails().getRole();
        switch (curRole) {
            case PRESIDENT:
            case SECRETARY:
            case ACCOUNTANT:
                break;
            case EMPLOYEE:
            default:
                if (!OrderStatus.IN_PROGRESS.equals(currentOrder.getStatus())) {
                    throw new BusinessException(
                            String.format(
                                    "Current order %s status is %s. Only status in progress can be updated.",
                                    currentOrder.getCode(),
                                    currentOrder.getStatus()
                            )
                    );
                }
        }
    }
}
