package com.shop.tbms.util;

import com.shop.tbms.entity.TemplateChecklist;
import com.shop.tbms.entity.TemplateProcedure;
import com.shop.tbms.entity.TemplateStepSequence;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class TemplateUtil {
    public static List<TemplateStepSequence> getTemplateStepSequence(TemplateProcedure templateProcedure) {
        List<TemplateStepSequence> result = new ArrayList<>();
        templateProcedure.getListTemplateStep().forEach(templateStep -> {
            if (Boolean.TRUE.equals(templateStep.getIsEnd())) {
                result.addAll(templateStep.getListTemplateStepAfter());
            } else {
                result.addAll(templateStep.getListTemplateStepBefore());
            }
        });

        return result.stream().distinct().collect(Collectors.toList());
    }

    public static List<TemplateChecklist> getTemplateChecklist(TemplateProcedure templateProcedure) {
        List<TemplateChecklist> result = new ArrayList<>();
        templateProcedure.getListTemplateStep().forEach(templateStep -> {
            result.addAll(templateStep.getListTemplateChecklist());
        });

        return result;
    }
}
