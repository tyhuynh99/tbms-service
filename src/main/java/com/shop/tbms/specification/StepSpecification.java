package com.shop.tbms.specification;

import com.shop.tbms.entity.Step;
import com.shop.tbms.entity.Step_;
import com.shop.tbms.enumerate.step.StepStatus;
import org.springframework.data.jpa.domain.Specification;

import java.time.LocalDate;

import static com.shop.tbms.constant.AppConstant.NEARLY_LATE_DAYS;

public class StepSpecification {
    public static Specification<Step> getStepNearlyLate() {
        LocalDate today = LocalDate.now();
        LocalDate chkDate = today.plusDays(NEARLY_LATE_DAYS);

        Specification<Step> specification = Specification.where(null);
        return specification.and(
                (root, query, criteriaBuilder) ->
                        criteriaBuilder.notEqual(
                                root.get(Step_.STATUS),
                                StepStatus.COMPLETED)
        ).and(
                (root, query, criteriaBuilder) ->
                        criteriaBuilder.greaterThan(
                                root.get(Step_.EXPECTED_COMPLETE_DATE),
                                today)
        ).and(
                (root, query, criteriaBuilder) ->
                        criteriaBuilder.lessThanOrEqualTo(
                                root.get(Step_.EXPECTED_COMPLETE_DATE),
                                chkDate)
        );
    }

    public static Specification<Step> getStepLate() {
        LocalDate today = LocalDate.now();

        Specification<Step> specification = Specification.where(null);
        return specification.and(
                (root, query, criteriaBuilder) ->
                        criteriaBuilder.notEqual(
                                root.get(Step_.STATUS),
                                StepStatus.COMPLETED)
        ).and(
                (root, query, criteriaBuilder) ->
                        criteriaBuilder.lessThan(
                                root.get(Step_.EXPECTED_COMPLETE_DATE),
                                today)
        );
    }
}
