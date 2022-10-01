package com.shop.tbms.mapper.account;

import com.shop.tbms.dto.account.CreateAccountReqDTO;
import com.shop.tbms.entity.Account;
import com.shop.tbms.entity.Position;
import com.shop.tbms.util.PasswordUtil;
import org.apache.commons.lang3.StringUtils;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

@Mapper(componentModel = "spring")
public interface CreateAccountMapper {
    @Mapping(target = "active", source = "reqDTO", qualifiedByName = "setActive")
    @Mapping(target = "password", source = "password", qualifiedByName = "generatePassword")
    @Mapping(target = "position", source = "positionCode", qualifiedByName = "generatePosition")
    Account toAccountEntity(CreateAccountReqDTO reqDTO);

    @Named("generatePosition")
    default Position generatePosition(String positionCode) {
        if (!StringUtils.isBlank(positionCode)) {
            Position position = new Position();
            position.setCode(positionCode);
            return position;
        }
        return null;
    }

    @Named("setActive")
    default Boolean setActive(CreateAccountReqDTO reqDTO) {
        return true;
    }

    @Named("generatePassword")
    default String generatePassword(String password) {
        return PasswordUtil.encodePassword(password);
    }
}
