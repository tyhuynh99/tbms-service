package com.shop.tbms.component;

import com.shop.tbms.entity.Step;
import com.shop.tbms.entity.StepSequence;
import com.shop.tbms.entity.TemplateStepSequence;
import com.shop.tbms.mapper.procedure.ProcedureFromTemplateMapper;
import com.shop.tbms.util.StepUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class StepSequenceComponent {
    @Autowired
    private ProcedureFromTemplateMapper procedureFromTemplateMapper;

    public List<StepSequence> generateStepSequence(List<Step> listCurrentStep, List<TemplateStepSequence> listTemplateStepSequence) {
        /* Generate StepSequence from template */
        List<StepSequence> listStepSequence = procedureFromTemplateMapper.fromTemplateStepSequence(listTemplateStepSequence);

        /* Map Step ID to StepSequence */
        return listStepSequence.stream().map(stepSequence -> {
            Step stepAfter = StepUtil.findStepByStepCode(stepSequence.getStepAfter().getCode(), listCurrentStep);
            Step stepBefore = StepUtil.findStepByStepCode(stepSequence.getStepBefore().getCode(), listCurrentStep);

            stepSequence.setStepAfter(stepAfter);
            stepSequence.setStepBefore(stepBefore);

            return stepSequence;
        }).collect(Collectors.toList());
    }
}
