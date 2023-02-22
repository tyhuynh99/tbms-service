package com.shop.tbms.specification;

import com.shop.tbms.dto.order.OrderFilterReqDTO;
import com.shop.tbms.entity.Procedure;
import com.shop.tbms.entity.Procedure_;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.entity.PurchaseOrder_;
import com.shop.tbms.enumerate.order.OrderStatus;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import java.time.LocalDate;
import java.util.Objects;

import static com.shop.tbms.constant.AppConstant.NEARLY_LATE_DAYS;

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

    public static Specification<PurchaseOrder> getListLateOrderOtherDay() {
        /* delivery date = today */
        Specification<PurchaseOrder> specification = Specification
                .where(
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

        /* is late */
        specification = specification
                .and(
                        (root, query, criteriaBuilder) ->
                                criteriaBuilder.equal(
                                        root.get(PurchaseOrder_.IS_LATE),
                                        Boolean.TRUE)
                );
        return specification;
    }

    public static Specification<PurchaseOrder> getListNearlyDueOrder() {
        Specification<PurchaseOrder> specification = Specification.where(null);
        LocalDate today = LocalDate.now();
        LocalDate chkDate = LocalDate.now().plusDays(NEARLY_LATE_DAYS);

        specification = specification.and(
                (root, query, criteriaBuilder) ->
                        criteriaBuilder.notEqual(
                                root.get(PurchaseOrder_.STATUS),
                                OrderStatus.COMPLETED)
        ).and( /* is delete flase */
                (root, query, criteriaBuilder) ->
                        criteriaBuilder.equal(
                                root.get(PurchaseOrder_.IS_DELETED),
                                Boolean.FALSE)
        ).and( /* deliver date > today */
                (root, query, criteriaBuilder) ->
                        criteriaBuilder.greaterThan(
                                root.get(PurchaseOrder_.DELIVERED_DATE),
                                today)
        ).and( /* deliver date <= today + 2 days */
                (root, query, criteriaBuilder) ->
                        criteriaBuilder.lessThanOrEqualTo(
                                root.get(PurchaseOrder_.DELIVERED_DATE),
                                chkDate)
        );
        return specification;
    }
}
