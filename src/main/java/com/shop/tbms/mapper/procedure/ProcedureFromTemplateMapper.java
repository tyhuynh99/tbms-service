package com.shop.tbms.mapper.procedure;

import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.step.StepStatus;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ProcedureFromTemplateMapper {
    /* PROCEDURE */
    Procedure fromTemplateStep(TemplateProcedure templateProcedure);

    /* STEP */
    @Mapping(target = "status", source = "templateStep", qualifiedByName = "getDefaultStatus")
    Step fromTemplateStep(TemplateStep templateStep);

    List<Step> fromTemplateStep(List<TemplateStep> templateStepList);

    /* STEP SEQUENCE */
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "stepBefore", source = "templateStepBefore", qualifiedByName = "getStepBeforeFromTemplate")
    @Mapping(target = "stepAfter", source = "templateStepAfter", qualifiedByName = "getStepAfterFromTemplate")
    StepSequence fromTemplateStepSequence(TemplateStepSequence templateStepSequence);

    List<StepSequence> fromTemplateStepSequence(List<TemplateStepSequence> templateStepSequenceList);
    
    /* CHECKLIST */
    @Mapping(target = "step", source = "templateStep", qualifiedByName = "getStepForChecklist")
    @Mapping(target = "isChecked", constant = "false")
    @Mapping(target = "id", ignore = true)
    Checklist fromTemplateChecklist(TemplateChecklist templateChecklist);

    List<Checklist> fromTemplateChecklist(List<TemplateChecklist> templateChecklistList);

    @Named("getDefaultStatus")
    default StepStatus getDefaultStatus(TemplateStep templateStep) {
        return StepStatus.INIT;
    }

    @Named("getStepBeforeFromTemplate")
    default Step getStepBeforeFromTemplate(TemplateStep templateStepBefore) {
        return fromTemplateStep(templateStepBefore);
    }

    @Named("getStepAfterFromTemplate")
    default Step getStepAfterFromTemplate(TemplateStep templateStepAfter) {
        return fromTemplateStep(templateStepAfter);
    }
    
    @Named("getStepForChecklist")
    default Step getStepForChecklist(TemplateStep templateStep) {
        return fromTemplateStep(templateStep);
    }
}
