package com.shop.tbms.mapper;

import com.shop.tbms.dto.step.detail.MoldProgressInStepDTO;
import com.shop.tbms.entity.MoldProgress;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import java.util.List;

@Mapper(componentModel = "spring")
public interface MoldProgressMapper {
    @Mapping(target = "progressId", source = "id")
    @Mapping(target = "moldId", source = "mold.id")
    @Mapping(target = "moldSize", source = "mold.size")
    MoldProgressInStepDTO toDTO(MoldProgress moldProgress);

    List<MoldProgressInStepDTO> toDTOs(List<MoldProgress> moldProgresses);
}
