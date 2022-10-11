package com.shop.tbms.mapper.mold;

import com.shop.tbms.dto.mold.MoldElementDTO;
import com.shop.tbms.entity.MoldGroupElement;
import org.mapstruct.Mapper;

import java.util.List;

@Mapper(componentModel = "spring")
public interface MoldGroupElementMapper {
    MoldElementDTO toDTO(MoldGroupElement moldGroupElement);

    List<MoldElementDTO> toDTOs(List<MoldGroupElement> listEntities);
}
