package com.shop.tbms.specification;

import com.shop.tbms.entity.Account;
import com.shop.tbms.entity.Account_;
import com.shop.tbms.entity.Position_;
import com.shop.tbms.enumerate.Role;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;

public class AccountSpecification {
    public static Specification<Account> genGetListEmployee() {
        Specification<Account> specification = Specification.where(
                (root, query, criteriaBuilder) ->  criteriaBuilder.equal(root.get(Account_.ROLE), Role.EMPLOYEE)
        );

        specification = specification.and(
                (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(Account_.ACTIVE), Boolean.TRUE)
        );

        return specification;
    }

    public static Specification<Account> genGetListByRole(List<Role> roleList) {
        Specification<Account> specification = Specification.where(
                (root, query, criteriaBuilder) ->  root.get(Account_.ROLE).in(roleList)
        );

        specification = specification.and(
                (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(Account_.ACTIVE), Boolean.TRUE)
        );

        return specification;
    }

    public static Specification<Account> genGetListByRoleAndPosition(List<Role> roleList, List<String> positionCodeList) {
        Specification<Account> specification = Specification.where(
                (root, query, criteriaBuilder) ->  root.get(Account_.ROLE).in(roleList)
        );

        specification = specification.and(
                (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(Account_.ACTIVE), Boolean.TRUE)
        ).and(
                (root, query, criteriaBuilder) -> root.get(Account_.POSITION + "." + Position_.CODE).in(positionCodeList));

        return specification;
    }
}
