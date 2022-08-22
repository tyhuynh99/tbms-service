package com.shop.tbms.mapper.procedure;

import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.StepStatus;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

@Mapper(componentModel = "spring")
public interface ProcedureFromTemplateMapper {
    /* PROCEDURE */
    Procedure fromTemplate(TemplateProcedure templateProcedure);

    /* STEP */
    @Mapping(target = "status", source = "templateStep", qualifiedByName = "getDefaultStatus")
    Step fromTemplate(TemplateStep templateStep);

    /* STEP SEQUENCE */
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "stepBefore", source = "templateStepBefore", qualifiedByName = "getStepBeforeFromTemplate")
    @Mapping(target = "stepAfter", source = "templateStepAfter", qualifiedByName = "getStepAfterFromTemplate")
    StepSequence fromTemplate(TemplateStepSequence templateStepSequence);

    @Named("getDefaultStatus")
    default StepStatus getDefaultStatus(TemplateStep templateStep) {
        return StepStatus.INIT;
    }

    @Named("getStepBeforeFromTemplate")
    default Step getStepBeforeFromTemplate(TemplateStep templateStepBefore) {
        return fromTemplate(templateStepBefore);
    }

    @Named("getStepAfterFromTemplate")
    default Step getStepAfterFromTemplate(TemplateStep templateStepAfter) {
        return fromTemplate(templateStepAfter);
    }
}
