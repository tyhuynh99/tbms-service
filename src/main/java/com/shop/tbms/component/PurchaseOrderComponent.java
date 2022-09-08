package com.shop.tbms.component;

import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.dto.order.OrderFilterReqDTO;
import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.OrderStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import java.util.List;
import java.util.Objects;

@Component
public class PurchaseOrderComponent {
    public void canUpdateOrder(PurchaseOrder currentOrder) {
        /* check order status */
        if (!OrderStatus.IN_PROGRESS.equals(currentOrder.getStatus())) {
            throw new BusinessException(
                    String.format(
                            "Current order {} status is {}. Only status in progress can be updated.",
                            currentOrder.getCode(),
                            currentOrder.getStatus()
                    )
            );
        }
    }
}
