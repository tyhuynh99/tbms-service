package com.shop.tbms.component;

import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.dto.order.OrderFilterReqDTO;
import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.OrderStatus;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import java.util.Objects;

@Component
public class PurchaseOrderComponent {
    public Specification<PurchaseOrder> buildSpecForList(OrderFilterReqDTO orderFilterReqDTO) {
        Specification<PurchaseOrder> specification = Specification.where(null);

        if (Objects.nonNull(orderFilterReqDTO.getCodeContains())) {
            specification = specification.and((root, query, criteriaBuilder) ->
                    criteriaBuilder.like(
                            root.get(PurchaseOrder_.code),
                            "%" + orderFilterReqDTO.getCodeContains() + "%"));
        }

        if (Objects.nonNull(orderFilterReqDTO.getProcedureCodeEqual())) {
            specification = specification.and((root, query, criteriaBuilder) -> {
                Join<PurchaseOrder, Procedure> join = root.join(PurchaseOrder_.PROCEDURE, JoinType.INNER);

                return criteriaBuilder.equal(
                        join.get(Procedure_.code),
                        orderFilterReqDTO.getProcedureCodeEqual());
            });
        }

        if (Objects.nonNull(orderFilterReqDTO.getOrderStatusEqual())) {
            specification = specification.and((root, query, criteriaBuilder) ->
                    criteriaBuilder.equal(
                            root.get(PurchaseOrder_.STATUS),
                            orderFilterReqDTO.getOrderStatusEqual()));
        }

        if (Objects.nonNull(orderFilterReqDTO.getOrderPaymentStatusEqual())) {
            specification = specification.and((root, query, criteriaBuilder) ->
                    criteriaBuilder.equal(
                            root.get(PurchaseOrder_.PAYMENT_STATUS),
                            orderFilterReqDTO.getOrderPaymentStatusEqual()));
        }

        return specification;
    }

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
