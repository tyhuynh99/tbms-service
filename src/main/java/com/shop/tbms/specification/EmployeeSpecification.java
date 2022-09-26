package com.shop.tbms.specification;

import com.shop.tbms.entity.Account;
import com.shop.tbms.entity.Account_;
import com.shop.tbms.enumerate.Role;
import org.springframework.data.jpa.domain.Specification;

public class EmployeeSpecification {
    public static Specification<Account> genGetListEmployee() {
        Specification<Account> specification = Specification.where(
                (root, query, criteriaBuilder) ->  criteriaBuilder.equal(root.get(Account_.ROLE), Role.EMPLOYEE)
        );

        specification = specification.and(
                (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(Account_.ACTIVE), Boolean.TRUE)
        );

        return specification;
    }
}
