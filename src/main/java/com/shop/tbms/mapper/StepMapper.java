package com.shop.tbms.mapper;

import com.shop.tbms.dto.UserAssignedStepDTO;
import com.shop.tbms.dto.step.detail.StepDTO;
import com.shop.tbms.entity.Step;
import com.shop.tbms.entity.TemplateStep;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import java.util.List;

@Mapper(componentModel = "spring", uses = {ChecklistMapper.class, EvidenceMapper.class, MoldElementMapper.class, MoldProgressMapper.class})
public interface StepMapper {
    @Mapping(target = "checklist", source = "listChecklist")
    @Mapping(target = "listMoldProgress", source = "listMoldProgresses")
    @Mapping(target = "listMoldElement", source = "procedure.purchaseOrder.listMoldElement")
    @Mapping(target = "resettable", defaultValue = "false")
    @Mapping(target = "requiredEvidence", defaultValue = "false")
    @Mapping(target = "isStart", defaultValue = "false")
    @Mapping(target = "isEnd", defaultValue = "false")
    @Mapping(target = "isPaid", ignore = true, defaultValue = "false")
    StepDTO toDTO(Step step);

    List<StepDTO> toDTOs(List<Step> steps);

    @Mapping(target = "stepCode", source = "code")
    @Mapping(target = "stepName", source = "name")
    UserAssignedStepDTO toUserStep(TemplateStep step);

    List<UserAssignedStepDTO> toUserSteps(List<TemplateStep> steps);
}
