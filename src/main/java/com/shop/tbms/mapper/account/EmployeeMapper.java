package com.shop.tbms.mapper.account;

import com.shop.tbms.dto.account.EmployeeInListDTO;
import com.shop.tbms.entity.Account;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import java.util.List;

@Mapper(componentModel = "spring")
public interface EmployeeMapper {
    @Mapping(target = "position", source = "position.name")
    EmployeeInListDTO toListEmployee(Account account);

    List<EmployeeInListDTO> toListEmployees(List<Account> accounts);
}
