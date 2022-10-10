package com.shop.tbms.mapper.progress;

import com.shop.tbms.dto.step.detail.progress.MoldDeliverProgressDTO;
import com.shop.tbms.entity.MoldDeliverProgress;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import java.util.List;

@Mapper(componentModel = "spring")
public interface MoldDeliverProgressMapper {
    @Mapping(target = "progressId", source = "id")
    @Mapping(target = "moldSize", source = "mold.size")
    @Mapping(target = "canCheck", constant = "true")
    MoldDeliverProgressDTO toDTO(MoldDeliverProgress entity);

    List<MoldDeliverProgressDTO> toDTOs(List<MoldDeliverProgress> entities);
}
