package com.shop.tbms.component;

import com.shop.tbms.entity.*;
import com.shop.tbms.mapper.procedure.ProcedureFromTemplateMapper;
import com.shop.tbms.repository.TemplateProcedureRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.persistence.EntityNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Component
public class ProcedureComponent {
    @Autowired
    private TemplateProcedureRepository templateProcedureRepository;

    @Autowired
    private ProcedureFromTemplateMapper procedureFromTemplateMapper;

    public TemplateProcedure getTemplateProcedure(String templateProcedureCode) {
        return templateProcedureRepository.findById(templateProcedureCode)
                .orElseThrow(() ->
                        new EntityNotFoundException("Not found procedure template with code " + templateProcedureCode));
    }

    public Procedure generateProcedureFromTemplate(TemplateProcedure templateProcedure) {
        /* Generate Procedure from template */
        Procedure procedure = procedureFromTemplateMapper.fromTemplateStep(templateProcedure);

        /* Generate Step from template */
        List<Step> listStep = procedureFromTemplateMapper.fromTemplateStep(templateProcedure.getListTemplateStep());

        /* Set relation of Procedure and Step */
        procedure.setListStep(listStep);
        listStep.forEach(step -> {
            step.setProcedure(procedure);
        });

        return procedure;
    }
}
