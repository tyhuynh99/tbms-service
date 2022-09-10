package com.shop.tbms.util;

import com.shop.tbms.entity.Step;
import com.shop.tbms.entity.StepSequence;
import org.springframework.util.CollectionUtils;

import javax.persistence.EntityNotFoundException;
import java.util.Comparator;
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

    public static Step getNextMainStep(List<StepSequence> lisStepSequenceBefore) {
        if (CollectionUtils.isEmpty(lisStepSequenceBefore)) return null;

        lisStepSequenceBefore.sort(Comparator.comparing(o -> o.getStepAfter().getSequenceNo()));
        return lisStepSequenceBefore.get(lisStepSequenceBefore.size() - 1).getStepAfter();
    }
}
