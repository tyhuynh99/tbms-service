package com.shop.tbms.mapper.mold;

import com.shop.tbms.dto.mold.MoldDTO;
import com.shop.tbms.entity.Mold;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import java.util.List;
import java.util.Objects;

@Mapper(componentModel = "spring")
public interface MoldMapper {
    @Mapping(target = "sizeWithType", source = "mold", qualifiedByName = "getSizeWithType")
    MoldDTO toDTO(Mold mold);

    List<MoldDTO> toDTOs(List<Mold> molds);

    @Named("getSizeWithType")
    default String getSizeWithType(Mold mold) {
        if (Objects.nonNull(mold.getMoldGroup())) {
            return mold.getSize() + "#" + mold.getMoldGroup().getType().name();
        }
        return mold.getSize();
    }
}
