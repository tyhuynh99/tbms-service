package com.shop.tbms.specification;

import com.shop.tbms.dto.order.OrderFilterReqDTO;
import com.shop.tbms.entity.Procedure;
import com.shop.tbms.entity.Procedure_;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.entity.PurchaseOrder_;
import com.shop.tbms.enumerate.OrderStatus;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import java.time.LocalDate;
import java.util.Objects;

public class PurchaseOrderSpecification {
    public static Specification<PurchaseOrder> buildSpecForList(OrderFilterReqDTO orderFilterReqDTO) {
        /* order not deleted */
        Specification<PurchaseOrder> specification = Specification
                .where(
                        (root, query, criteriaBuilder) ->
                                criteriaBuilder.equal(
                                        root.get(PurchaseOrder_.IS_DELETED),
                                        Boolean.FALSE)
                );

        if (Objects.nonNull(orderFilterReqDTO.getCodeContains())) {
            specification = specification.and((root, query, criteriaBuilder) ->
                    criteriaBuilder.like(
                            criteriaBuilder.upper(root.get(PurchaseOrder_.code)),
                            "%" + orderFilterReqDTO.getCodeContains().toUpperCase() + "%"));
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

    public static Specification<PurchaseOrder> getListLateOrderToday() {
        /* delivery date = today */
        Specification<PurchaseOrder> specification = Specification
                .where(
                        (root, query, criteriaBuilder) ->
                                criteriaBuilder.equal(
                                        root.get(PurchaseOrder_.DELIVERED_DATE),
                                        LocalDate.now())
                );

        /* status in progress */
        specification = specification
                .and(
                        (root, query, criteriaBuilder) ->
                                criteriaBuilder.equal(
                                        root.get(PurchaseOrder_.STATUS),
                                        OrderStatus.IN_PROGRESS)
                );

        /* not deleted */
        specification = specification
                .and(
                        (root, query, criteriaBuilder) ->
                                criteriaBuilder.equal(
                                        root.get(PurchaseOrder_.IS_DELETED),
                                        Boolean.FALSE)
                );
        return specification;
    }
}
