package com.shop.tbms.mapper;

import com.shop.tbms.dto.step.report_error.ReportErrorToStepRespDTO;
import com.shop.tbms.entity.StepSequence;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import java.util.List;

@Mapper(componentModel = "spring")
public interface StepSequenceMapper {
    @Mapping(target = "stepId", source = "stepAfter.id")
    @Mapping(target = "stepName", source = "stepAfter.name")
    @Mapping(target = "stepCode", source = "stepAfter.code")
    ReportErrorToStepRespDTO toToNextStepError(StepSequence stepSequence);

    List<ReportErrorToStepRespDTO> toToNextStepErrors(List<StepSequence> steps);
}
