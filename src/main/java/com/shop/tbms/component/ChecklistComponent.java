package com.shop.tbms.component;

import com.shop.tbms.entity.Checklist;
import com.shop.tbms.entity.Step;
import com.shop.tbms.entity.StepSequence;
import com.shop.tbms.entity.TemplateChecklist;
import com.shop.tbms.mapper.procedure.ProcedureFromTemplateMapper;
import com.shop.tbms.util.StepUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class ChecklistComponent {
    @Autowired
    private ProcedureFromTemplateMapper procedureFromTemplateMapper;

    public List<Checklist> generateChecklist(List<Step> listCurrentStep, List<TemplateChecklist> listTemplateChecklist) {
        /* Generate Checklist from template */
        List<Checklist> listChecklist = procedureFromTemplateMapper.fromTemplateChecklist(listTemplateChecklist);

        /* Map Step ID to Checklist */
        return listChecklist.stream().map(checklist -> {
            Step step = StepUtil.getIdOfStep(checklist.getStep().getCode(), listCurrentStep);
            checklist.setStep(step);

            return checklist;
        }).collect(Collectors.toList());
    }
}
