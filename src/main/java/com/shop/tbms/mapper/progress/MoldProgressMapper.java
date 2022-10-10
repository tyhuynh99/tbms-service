package com.shop.tbms.mapper.progress;

import com.shop.tbms.dto.step.detail.progress.MoldProgressDTO;
import com.shop.tbms.entity.MoldProgress;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import java.util.List;

@Mapper(componentModel = "spring")
public interface MoldProgressMapper {
    @Mapping(target = "progressId", source = "id")
    @Mapping(target = "moldId", source = "mold.id")
    @Mapping(target = "moldSize", source = "mold.size")
    @Mapping(target = "canCheck", constant = "true")
    MoldProgressDTO toDTO(MoldProgress moldProgress);

    List<MoldProgressDTO> toDTOs(List<MoldProgress> moldProgresses);
}
