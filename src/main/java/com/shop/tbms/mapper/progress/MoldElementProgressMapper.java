package com.shop.tbms.mapper.progress;

import com.shop.tbms.dto.step.detail.progress.MoldElementProgressDetailDTO;
import com.shop.tbms.entity.MoldGroupElementProgress;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import java.util.List;

@Mapper(componentModel = "spring")
public interface MoldElementProgressMapper {
    @Mapping(target = "progressId", source = "id")
    @Mapping(target = "elementName", source = "moldGroupElement.name")
    @Mapping(target = "canCheck", constant = "false")
    @Mapping(target = "canUncheck", constant = "false")
    MoldElementProgressDetailDTO toDTO(MoldGroupElementProgress entity);

    List<MoldElementProgressDetailDTO> toDTOs(List<MoldGroupElementProgress> entities);
}
