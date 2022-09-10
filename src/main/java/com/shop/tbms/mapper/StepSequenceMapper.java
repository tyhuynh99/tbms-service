package com.shop.tbms.mapper;

import com.shop.tbms.dto.step.report_issue.ReportIssueToStepRespDTO;
import com.shop.tbms.entity.StepSequence;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import java.util.List;

@Mapper(componentModel = "spring")
public interface StepSequenceMapper {
    @Mapping(target = "stepId", source = "stepAfter.id")
    @Mapping(target = "stepName", source = "stepAfter.name")
    @Mapping(target = "stepCode", source = "stepAfter.code")
    ReportIssueToStepRespDTO toToNextStepError(StepSequence stepSequence);

    List<ReportIssueToStepRespDTO> toToNextStepErrors(List<StepSequence> steps);
}
