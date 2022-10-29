package com.shop.tbms.mapper.mold;

import com.shop.tbms.dto.mold.MoldGroupDetailDTO;
import com.shop.tbms.entity.Mold;
import com.shop.tbms.entity.MoldGroup;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;
import org.mapstruct.Named;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Mapper(componentModel = "spring", uses = {MoldGroupElementMapper.class})
public interface MoldGroupDetailMapper {
    @Mapping(target = "moldList", source = "moldGroup", qualifiedByName = "getMoldList")
    MoldGroupDetailDTO toDTO(MoldGroup moldGroup);

    List<MoldGroupDetailDTO> toDTOs(List<MoldGroup> moldGroupList);

    @Named("getMoldList")
    default List<String> getMoldList(MoldGroup moldGroup) {
        return Optional.ofNullable(moldGroup.getListMold())
                .orElse(new ArrayList<>())
                .stream()
                .map(Mold::getSize)
                .collect(Collectors.toList());
    }

    @Mapping(target = "id", ignore = true)
    void partialUpdate(@MappingTarget MoldGroup moldGroup, MoldGroupDetailDTO detailDTO);
}
