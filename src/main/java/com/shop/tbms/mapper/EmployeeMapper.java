package com.shop.tbms.mapper;

import com.shop.tbms.dto.account.EmployeeInListDTO;
import com.shop.tbms.entity.Account;
import com.shop.tbms.entity.AccountAssignStep;
import org.apache.commons.lang3.StringUtils;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Mapper(componentModel = "spring")
public interface EmployeeMapper {
    @Mapping(target = "assignedStep", source = "assignedSteps", qualifiedByName = "genAssignedStep")
    EmployeeInListDTO toListEmployee(Account account);

    List<EmployeeInListDTO> toListEmployees(List<Account> accounts);

    @Named("genAssignedStep")
    default String genAssignedStep(List<AccountAssignStep> assignedSteps) {
        if (Objects.nonNull(assignedSteps)) {
            return assignedSteps.stream()
                    .map(AccountAssignStep::getStep)
                    .collect(Collectors.joining(", "));
        }
        return StringUtils.EMPTY;
    }
}
