package com.shop.tbms.util;

import com.shop.tbms.entity.Step;

import javax.persistence.EntityNotFoundException;
import java.util.List;

public class StepUtil {
    public static Step getIdOfStep(String stepCode, List<Step> listStep) {
        return listStep.stream()
                .filter(step -> step.getCode().equals(stepCode))
                .findFirst()
                .orElseThrow(EntityNotFoundException::new);
    }
}
