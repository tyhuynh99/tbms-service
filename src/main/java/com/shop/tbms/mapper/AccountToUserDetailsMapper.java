package com.shop.tbms.mapper;

import com.shop.tbms.config.security.TbmsUserDetails;
import com.shop.tbms.entity.Account;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

@Mapper(componentModel = "spring")
public interface AccountToUserDetailsMapper {
    @Mapping(target = "userId", source = "id")
    TbmsUserDetails toUserDetails(Account account);
}
