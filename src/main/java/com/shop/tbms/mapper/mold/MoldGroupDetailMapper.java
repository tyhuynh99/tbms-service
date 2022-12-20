package com.shop.tbms.mapper.mold;

import com.shop.tbms.dto.mold.MoldGroupDetailReqDTO;
import com.shop.tbms.dto.mold.MoldGroupDetailResDTO;
import com.shop.tbms.entity.MoldGroup;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;

import java.util.List;

@Mapper(componentModel = "spring", uses = {MoldGroupElementMapper.class, MoldMapper.class})
public interface MoldGroupDetailMapper {
    @Mapping(target = "moldElementList", source = "listMoldGroupElement")
    @Mapping(target = "moldList", source = "listMold")
    MoldGroupDetailResDTO toDTO(MoldGroup moldGroup);

    List<MoldGroupDetailResDTO> toDTOs(List<MoldGroup> moldGroupList);

    @Mapping(target = "moldElementList", source = "listMoldGroupElement")
    MoldGroupDetailReqDTO toReqDTO(MoldGroup moldGroup);

    List<MoldGroupDetailReqDTO> toReqDTOs(List<MoldGroup> moldGroupList);

    @Mapping(target = "id", ignore = true)
    void partialUpdate(@MappingTarget MoldGroup moldGroup, MoldGroupDetailReqDTO detailDTO);
}
