package com.shop.tbms.util;

import com.shop.tbms.entity.*;
import org.springframework.util.CollectionUtils;

import javax.persistence.EntityNotFoundException;
import java.util.Comparator;
import java.util.List;

import static com.shop.tbms.constant.AppConstant.ZERO_LONG;

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

    public static long calPercentComplete(Step step) {
        switch (step.getReportType()) {
            case BY_MOLD:
                return calPercentByMold(step);
            case BY_MOLD_ELEMENT:
                return calPercentByMoldGroupElement(step);
            case BY_MOLD_SEND_RECEIVE:
                return calPercentByMoldDeliver(step);
            default:
                return ZERO_LONG;
        }
    }

    public static long calPercentByMold(Step step) {
        List<MoldProgress> listMoldProgress = step.getListMoldProgress();

        if (CollectionUtils.isEmpty(listMoldProgress)) return ZERO_LONG;

        long completedMold = listMoldProgress.stream()
                .filter(moldProgress -> Boolean.TRUE.equals(moldProgress.getIsCompleted()))
                .count();
        long totalMold = listMoldProgress.size();

        return Math.floorDiv(completedMold, totalMold);
    }

    public static long calPercentByMoldGroupElement(Step step) {
        List<MoldGroupElementProgress> listMoldGroupElementProgresses = step.getListMoldGroupElementProgresses();

        if (CollectionUtils.isEmpty(listMoldGroupElementProgresses)) return ZERO_LONG;

        long completedElement = listMoldGroupElementProgresses.stream()
                .filter(moldGroupElementProgress -> Boolean.TRUE.equals(moldGroupElementProgress.getIsCompleted()))
                .count();
        long totalElement = listMoldGroupElementProgresses.size();

        return Math.floorDiv(completedElement, totalElement);
    }

    public static long calPercentByMoldDeliver(Step step) {
        List<MoldDeliverProgress> listMoldDeliverProgress = step.getListMoldDeliverProgress();

        if (CollectionUtils.isEmpty(listMoldDeliverProgress)) return ZERO_LONG;

        long completedMoldDeliver = listMoldDeliverProgress.stream()
                .filter(moldDeliverProgress -> Boolean.TRUE.equals(moldDeliverProgress.getIsCompleted()))
                .count();
        long totalMoldDeliver = listMoldDeliverProgress.size();

        return Math.floorDiv(completedMoldDeliver, totalMoldDeliver);
    }
}
