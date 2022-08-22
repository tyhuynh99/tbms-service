package com.shop.tbms.component;

import com.shop.tbms.entity.Procedure;
import com.shop.tbms.entity.TemplateProcedure;
import com.shop.tbms.repository.TemplateProcedureRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.persistence.EntityNotFoundException;

@Component
public class ProcedureComponent {
    @Autowired
    private TemplateProcedureRepository templateProcedureRepository;

    public Procedure generateProcedureFromTemplate(String templateProcedureCode) {
        TemplateProcedure templateProcedure = templateProcedureRepository.findById(templateProcedureCode)
                .orElseThrow(() ->
                        new EntityNotFoundException("Not found procedure template with code " + templateProcedureCode));

        /* Generate Procedure from template */

        /* Generate Step from template */

        /* Set relation of Procedure and Step */

        return null;
    }
}
