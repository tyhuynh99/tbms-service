package com.shop.tbms.mapper;

import com.shop.tbms.dto.step.detail.StepDTO;
import com.shop.tbms.entity.Step;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import java.util.List;

@Mapper(componentModel = "spring", uses = {ChecklistMapper.class, EvidenceMapper.class, MoldElementMapper.class, MoldProgressMapper.class})
public interface StepMapper {
    @Mapping(target = "checklist", source = "listChecklist")
    @Mapping(target = "listMoldProgress", source = "listMoldProgresses")
    @Mapping(target = "listMoldElement", source = "procedure.purchaseOrder.listMoldElement")
    StepDTO toDTO(Step step);

    List<StepDTO> toDTOs(List<Step> steps);
}
