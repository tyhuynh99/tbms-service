package com.shop.tbms.specification;

import com.shop.tbms.entity.monitor.MonitorScheduler;
import com.shop.tbms.entity.monitor.MonitorScheduler_;
import com.shop.tbms.enumerate.SchedulerFunction;
import org.springframework.data.jpa.domain.Specification;

import java.time.LocalDate;
import java.time.LocalTime;

public class MonitorSchedulerSpecification {
    public static Specification<MonitorScheduler> createCheckLateOrder() {
        Specification<MonitorScheduler> specification = Specification.where(null);

        /* query by execute date */
        LocalDate today = LocalDate.now();
        specification = specification.and(
                (root, query, criteriaBuilder) ->
                        criteriaBuilder
                                .between(
                                        root.get(MonitorScheduler_.CREATED_DATE),
                                        today.atStartOfDay(), today.atTime(LocalTime.MAX))
        );

        /* query by function id */
        specification = specification.and(
                (root, query, criteriaBuilder) ->
                        criteriaBuilder
                                .equal(
                                        root.get(MonitorScheduler_.FUNCTION_NAME),
                                        SchedulerFunction.CHECK_LATE_ORDER)
        );

        return specification;
    }
}
