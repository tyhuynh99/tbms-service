package com.shop.tbms.util;

import com.shop.tbms.entity.Step;

import javax.persistence.EntityNotFoundException;
import java.util.List;

public class StepUtil {
    public static Step findStepByStepCode(String stepCode, List<Step> listStep) {
        return listStep.stream()
                .filter(step -> step.getCode().equals(stepCode))
                .findFirst()
                .orElseThrow(EntityNotFoundException::new);
    }

    public static Step findStepById(Long stepId, List<Step> listStep) {
        return listStep.stream()
                .filter(step -> step.getId().equals(stepId))
                .findFirst()
                .orElseThrow(EntityNotFoundException::new);
    }

    public static boolean canUpdatePaid(Step currentStep) {
        // TODO:
        return true;
    }

    public static boolean isThirdPartyStep(Step currentStep) {
        // TODO:
        return true;
    }
}
