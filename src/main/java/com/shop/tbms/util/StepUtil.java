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

    public static Step getNextMainStep(List<StepSequence> listStepSequenceBefore) {
        if (CollectionUtils.isEmpty(listStepSequenceBefore)) return null;

        listStepSequenceBefore.sort(Comparator.comparing(o -> o.getStepAfter().getSequenceNo()));
        return listStepSequenceBefore.get(listStepSequenceBefore.size() - 1).getStepAfter();
    }

    public static Step getPreMainStep(List<StepSequence> listStepSequenceAfter) {
        if (CollectionUtils.isEmpty(listStepSequenceAfter)) return null;

        listStepSequenceAfter.sort(Comparator.comparing(o -> o.getStepBefore().getSequenceNo()));
        return listStepSequenceAfter.get(0).getStepAfter();
    }
}
