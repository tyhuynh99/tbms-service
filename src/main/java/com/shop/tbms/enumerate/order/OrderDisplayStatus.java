package com.shop.tbms.enumerate.order;

import com.fasterxml.jackson.annotation.JsonValue;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.util.AuthenticationUtil;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDate;
import java.util.Objects;

import static com.shop.tbms.constant.AppConstant.NEARLY_LATE_DAYS;

@Slf4j
public enum OrderDisplayStatus {
    LATE(0),
    NEARLY_DEADLINE(1),
    PAID(2),
    NOT_PAID(3),
    NORMAL(4),
    OVERDUE_NOT_PAID(5),
    COMPLETE(6);

    private int value;

    OrderDisplayStatus(int value) {
        this.value = value;
    }

    @JsonValue
    public int getValue() {return this.value;}

    public static OrderDisplayStatus generate(PurchaseOrder order) {
        if (OrderStatus.IN_PROGRESS.equals(order.getStatus())) {
            LocalDate deliveredDate = order.getDeliveredDate();

            if (!LocalDate.now().isBefore(deliveredDate)) {
                log.info("Order {} is in progress, delivered date is after or equal today. Display status is LATE", order);
                setIsLate(order);
                return OrderDisplayStatus.LATE;
            } else if (!LocalDate.now().minusDays(NEARLY_LATE_DAYS).isBefore(deliveredDate)) {
                log.info("Order {} is in progress, delivered date is after or equal today minus 2 days. Display status is NEARLY_DEADLINE", order);
                setIsLate(order);
                return OrderDisplayStatus.NEARLY_DEADLINE;
            } else {
                log.info("Order {} is in progress, delivered date is before today minus 2 days. Display status is NORMAL", order);
                return OrderDisplayStatus.NORMAL;
            }
        } else {
            Role currentRole = AuthenticationUtil.getUserDetails().getRole();

            log.info("Request get status completed order {} from role {}", order, currentRole);
            switch (currentRole) {
                case EMPLOYEE:
                    return OrderDisplayStatus.COMPLETE;
                default:
                    if (Objects.isNull(order.getPaymentStatus())) return NOT_PAID;

                    switch (order.getPaymentStatus()) {
                        case PAID:
                            return OrderDisplayStatus.PAID;
                        case NOT_PAID:
                            return OrderDisplayStatus.NOT_PAID;
                        case OVERDUE_NOT_PAID:
                            return OrderDisplayStatus.OVERDUE_NOT_PAID;
                        default:
                            return null;
                    }
            }
        }
    }

    private static void setIsLate(PurchaseOrder order) {
        if (!Boolean.TRUE.equals(order.getIsLate())) {
            log.info("Set isLate = true for order {}", order);
            order.setIsLate(Boolean.TRUE);
        }
    }
}
