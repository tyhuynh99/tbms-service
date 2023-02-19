package com.shop.tbms.mapper;

import com.shop.tbms.dto.UserAssignedStepDTO;
import com.shop.tbms.dto.step.detail.StepDTO;
import com.shop.tbms.entity.Step;
import com.shop.tbms.entity.TemplateStep;
import com.shop.tbms.enumerate.order.OrderPaymentStatus;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import java.util.List;

@Mapper(componentModel = "spring", uses = {ChecklistMapper.class, EvidenceMapper.class})
public interface StepMapper {
    @Mapping(target = "checklist", source = "listChecklist")
    @Mapping(target = "resettable", defaultValue = "false")
    @Mapping(target = "requiredEvidence", defaultValue = "false")
    @Mapping(target = "isStart", defaultValue = "false")
    @Mapping(target = "isEnd", defaultValue = "false")
    @Mapping(target = "isPaid", source = "step", qualifiedByName = "setIsPaid")
    StepDTO toDTO(Step step);

    List<StepDTO> toDTOs(List<Step> steps);

    @Mapping(target = "stepCode", source = "code")
    @Mapping(target = "stepName", source = "name")
    UserAssignedStepDTO toUserStep(TemplateStep step);

    List<UserAssignedStepDTO> toUserSteps(List<TemplateStep> steps);

    @Named("setIsPaid")
    default Boolean setIsPaid(Step step) {
        return OrderPaymentStatus.PAID.equals(step.getProcedure().getPurchaseOrder().getPaymentStatus());
    }
}
