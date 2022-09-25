package com.shop.tbms.mapper;

import com.shop.tbms.dto.UserAssignedStepDTO;
import com.shop.tbms.dto.step.detail.StepDTO;
import com.shop.tbms.entity.Mold;
import com.shop.tbms.entity.MoldProgress;
import com.shop.tbms.entity.Step;
import com.shop.tbms.entity.TemplateStep;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Mapper(componentModel = "spring", uses = {ChecklistMapper.class, EvidenceMapper.class, MoldElementMapper.class, MoldProgressMapper.class})
public interface StepMapper {
    @Mapping(target = "checklist", source = "listChecklist")
    @Mapping(target = "listMoldProgress", source = "listMoldProgresses")
//    @Mapping(target = "listMold", source = "listMoldProgresses", qualifiedByName = "genListMold")
    @Mapping(target = "listMoldElement", source = "procedure.purchaseOrder.listMoldElement")
    @Mapping(target = "resettable", defaultValue = "false")
    @Mapping(target = "requiredEvidence", defaultValue = "false")
    @Mapping(target = "isStart", defaultValue = "false")
    @Mapping(target = "isEnd", defaultValue = "false")
    @Mapping(target = "isPaid", ignore = true, defaultValue = "false")
    StepDTO toDTO(Step step);

//    @Named("genListMold")
//    default List<String> genListMold(List<MoldProgress> listMoldProgress) {
//        if (Objects.nonNull(listMoldProgress)) {
//            return listMoldProgress.stream()
//                    .map(MoldProgress::getMold)
//                    .map(Mold::getSize)
//                    .collect(Collectors.toList());
//        }
//        return List.of();
//    }

    List<StepDTO> toDTOs(List<Step> steps);

    @Mapping(target = "stepCode", source = "code")
    @Mapping(target = "stepName", source = "name")
    UserAssignedStepDTO toUserStep(TemplateStep step);

    List<UserAssignedStepDTO> toUserSteps(List<TemplateStep> steps);
}
