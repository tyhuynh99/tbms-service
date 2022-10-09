package com.shop.tbms.mapper;

import com.shop.tbms.dto.mold.MoldGroupDetailDTO;
import com.shop.tbms.entity.MoldGroup;
import org.mapstruct.Mapper;

import java.util.List;

@Mapper(componentModel = "spring")
public interface MoldGroupDetailMapper {
    MoldGroupDetailDTO toDTO(MoldGroup moldGroup);

    List<MoldGroupDetailDTO> toDTOs(List<MoldGroup> moldGroupList);

}
