package com.shop.tbms.util;

import com.shop.tbms.entity.Mold;
import com.shop.tbms.entity.MoldDeliverProgress;
import com.shop.tbms.entity.MoldGroupElementProgress;
import com.shop.tbms.entity.MoldProgress;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.entity.Step;
import com.shop.tbms.entity.StepSequence;
import com.shop.tbms.enumerate.mold.MoldType;
import com.shop.tbms.enumerate.step.StepType;
import org.springframework.util.CollectionUtils;

import javax.persistence.EntityNotFoundException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

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

    public static List<Step> getNextStepToChkProgress(List<StepSequence> listStepSequenceBefore) {
        if (CollectionUtils.isEmpty(listStepSequenceBefore)) return List.of();

        Step currentStep = listStepSequenceBefore.get(0).getStepBefore();
        listStepSequenceBefore.sort(Comparator.comparing(o -> o.getStepAfter().getSequenceNo()));

        boolean hasFixingStep = listStepSequenceBefore.stream()
                .map(StepSequence::getStepAfter)
                .anyMatch(step -> StepType.FIXING.equals(step.getType()));

        if (hasFixingStep) {
            return listStepSequenceBefore.stream().map(StepSequence::getStepAfter)
                    .filter(step -> step.getSequenceNo() > currentStep.getSequenceNo())
                    .collect(Collectors.toList());
        }

        return List.of(getNextMainStep(listStepSequenceBefore));
    }

    public static Step getEndStep(PurchaseOrder order) {
        return order.getProcedure().getListStep().stream()
                .filter(step -> Boolean.TRUE.equals(step.getIsEnd()))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Not found end step of order " + order.getCode()));
    }

    public static Step getPreMainStep(List<StepSequence> listStepSequenceAfter) {
        if (CollectionUtils.isEmpty(listStepSequenceAfter)) return null;

        listStepSequenceAfter.sort(Comparator.comparing(o -> o.getStepBefore().getSequenceNo()));
        return listStepSequenceAfter.get(0).getStepBefore();
    }

    public static List<Step> getPreStepToChkProgress(List<StepSequence> listStepSequenceAfter) {
        if (CollectionUtils.isEmpty(listStepSequenceAfter)) return List.of();

        listStepSequenceAfter.sort(Comparator.comparing(o -> o.getStepBefore().getSequenceNo()));

        boolean hasFixingStep = listStepSequenceAfter.stream()
                .map(StepSequence::getStepBefore)
                .anyMatch(step -> StepType.FIXING.equals(step.getType()));

        if (hasFixingStep) {
            return listStepSequenceAfter.stream().map(StepSequence::getStepBefore).collect(Collectors.toList());
        }

        return List.of(getPreMainStep(listStepSequenceAfter));
    }

    public static Step getNextMainStep(List<StepSequence> listStepSequenceBefore, Mold mold) {
        if (CollectionUtils.isEmpty(listStepSequenceBefore)) return null;

        listStepSequenceBefore.sort(Comparator.comparing(o -> o.getStepAfter().getSequenceNo()));

        for (int i = listStepSequenceBefore.size() - 1; i >= 0; i--) {
            Step chkStep = listStepSequenceBefore.get(listStepSequenceBefore.size() - 1).getStepAfter();
            switch (chkStep.getReportType()) {
                case BY_MOLD:
                    if (Objects.nonNull(chkStep.getListMoldProgress())) {
                        boolean hasCurrMoldProgress = chkStep.getListMoldProgress().stream()
                                .anyMatch(moldProgress ->
                                        mold.getSize().equals(moldProgress.getMold().getSize()));

                        if (hasCurrMoldProgress) return chkStep;
                    }
                    break;
                case BY_MOLD_SEND_RECEIVE:
                    if (Objects.nonNull(chkStep.getListMoldDeliverProgress())) {
                        boolean hasCurrMoldProgress = chkStep.getListMoldDeliverProgress().stream()
                                .anyMatch(moldDeliverProgress ->
                                        mold.getSize().equals(moldDeliverProgress.getMold().getSize()));

                        if (hasCurrMoldProgress) return chkStep;
                    }
                    break;
                case BY_MOLD_ELEMENT:
                    if (Objects.nonNull(chkStep.getListMoldGroupElementProgresses())) {
                        boolean hasCurrMoldProgress = chkStep.getListMoldGroupElementProgresses().stream()
                                .anyMatch(moldGroupElementProgress ->
                                        mold.getSize().equals(moldGroupElementProgress.getMold().getSize()));

                        if (hasCurrMoldProgress) return chkStep;
                    }
                    break;
            }
        }
        return null;
    }

    public static Step getPreMainStep(List<StepSequence> listStepSequenceAfter, Mold mold) {
        if (CollectionUtils.isEmpty(listStepSequenceAfter)) return null;

        listStepSequenceAfter.sort(Comparator.comparing(o -> o.getStepBefore().getSequenceNo()));

        for (int i = 0; i < listStepSequenceAfter.size(); i++) {
            Step chkStep = listStepSequenceAfter.get(i).getStepBefore();
            switch (chkStep.getReportType()) {
                case BY_MOLD:
                    if (Objects.nonNull(chkStep.getListMoldProgress())) {
                        boolean hasCurrMoldProgress = chkStep.getListMoldProgress().stream()
                                .anyMatch(moldProgress ->
                                        mold.getSize().equals(moldProgress.getMold().getSize()));

                        if (hasCurrMoldProgress) return chkStep;
                    }
                    break;
                case BY_MOLD_SEND_RECEIVE:
                    if (Objects.nonNull(chkStep.getListMoldDeliverProgress())) {
                        boolean hasCurrMoldProgress = chkStep.getListMoldDeliverProgress().stream()
                                .anyMatch(moldDeliverProgress ->
                                        mold.getSize().equals(moldDeliverProgress.getMold().getSize()));

                        if (hasCurrMoldProgress) return chkStep;
                    }
                    break;
                case BY_MOLD_ELEMENT:
                    if (Objects.nonNull(chkStep.getListMoldGroupElementProgresses())) {
                        boolean hasCurrMoldProgress = chkStep.getListMoldGroupElementProgresses().stream()
                                .anyMatch(moldGroupElementProgress ->
                                        mold.getSize().equals(moldGroupElementProgress.getMold().getSize()));

                        if (hasCurrMoldProgress) return chkStep;
                    }
                    break;
            }
        }
        return null;
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
        if (!STEPS_FOR_FREEFORM.contains(step.getCode())) {
            listMoldProgress = listMoldProgress.stream().filter(x -> {
                if (!Objects.isNull(x.getMold().getMoldGroup())) {
                    return MoldType.FREEFORM.getValue() != x.getMold().getMoldGroup().getType().getValue();
                }
                return true;
            }).collect(Collectors.toList());
        }
        if (CollectionUtils.isEmpty(listMoldProgress)) return ZERO_LONG;

        long completedMold = listMoldProgress.stream()
                .filter(moldProgress -> Boolean.TRUE.equals(moldProgress.getIsCompleted()))
                .count();
        long totalMold = listMoldProgress.size();
        return (long) (((double) completedMold / totalMold) * 100);
    }

    private static final List<String> STEPS_NOT_LOI_DE = Arrays.asList("2D", "DAT_VAT_TU", "3D_KHUON", "CAM_KHUON", "CNC_KHUON", "HOP_KHUON", "RAP_KHUON", "PHONG_DIEN", "THU_KHUON", "SUA_KHUON", "TAO_HOA", "XI_MA_SON_DANH_BONG", "KIEM_TRA", "GIAO_HANG");
    private static final List<String> STEPS_FOR_FREEFORM = Arrays.asList("2D", "DAT_VAT_TU", "3D_KHUON", "CAM_KHUON", "CNC_KHUON", "HOP_KHUON", "RAP_KHUON", "THU_KHUON", "SUA_KHUON", "XI_MA_SON_DANH_BONG", "KIEM_TRA", "GIAO_HANG");

    public static long calPercentByMoldGroupElement(Step step) {
        List<MoldGroupElementProgress> listMoldGroupElementProgresses = step.getListMoldGroupElementProgresses();
        List<MoldProgress> moldProgressList = step.getListMoldProgress();

        List<Long> duplicateMolds = listMoldGroupElementProgresses.stream().map(x -> x.getMold().getId()).collect(Collectors.toList());
        if (!duplicateMolds.isEmpty()) {
            moldProgressList = moldProgressList.stream().filter(x -> !duplicateMolds.contains(x.getMold().getId())).collect(Collectors.toList());
        }

        if (!STEPS_NOT_LOI_DE.contains(step.getCode())) {
            if (!STEPS_FOR_FREEFORM.contains(step.getCode())) {
                moldProgressList = moldProgressList.stream()
                        .filter(x -> {
                            if (!Objects.isNull(x.getMold().getMoldGroup())) {
                                return MoldType.FREEFORM.getValue() != x.getMold().getMoldGroup().getType().getValue();
                            }
                            return true;
                        }).collect(Collectors.toList());
            }

            listMoldGroupElementProgresses = listMoldGroupElementProgresses.stream()
                    .filter(x -> {
                        if (!Objects.isNull(x.getMold().getMoldGroup())) {
                            return MoldType.FREEFORM.getValue() != x.getMold().getMoldGroup().getType().getValue();
                        }
                        return true;
                    }).collect(Collectors.toList());
        }

        if (CollectionUtils.isEmpty(listMoldGroupElementProgresses) && CollectionUtils.isEmpty(moldProgressList)) {
            return ZERO_LONG;
        }

        long completedElement = listMoldGroupElementProgresses.stream()
                .filter(moldGroupElementProgress -> Boolean.TRUE.equals(moldGroupElementProgress.getIsCompleted()))
                .count()
                +
                moldProgressList.stream().filter(x -> Boolean.TRUE.equals(x.getIsCompleted())).count();
        long totalElement = listMoldGroupElementProgresses.size() + moldProgressList.size();

        return totalElement > 0 ? (long) (((double) completedElement / totalElement) * 100) : 0L;
    }

    public static long calPercentByMoldDeliver(Step step) {
        List<MoldDeliverProgress> listMoldDeliverProgress = step.getListMoldDeliverProgress();
        if (!STEPS_FOR_FREEFORM.contains(step.getCode())) {
            listMoldDeliverProgress = listMoldDeliverProgress.stream()
                    .filter(x -> {
                        if (!Objects.isNull(x.getMold().getMoldGroup())) {
                            return MoldType.FREEFORM.getValue() != x.getMold().getMoldGroup().getType().getValue();
                        }
                        return true;
                    }).collect(Collectors.toList());
        }
        if (CollectionUtils.isEmpty(listMoldDeliverProgress)) return ZERO_LONG;

        long completedMoldDeliver = listMoldDeliverProgress.stream()
                .filter(moldDeliverProgress -> Boolean.TRUE.equals(moldDeliverProgress.getIsCompleted()))
                .count();
        long totalMoldDeliver = listMoldDeliverProgress.size();

        return totalMoldDeliver > 0 ? (long) (((double) completedMoldDeliver / totalMoldDeliver) * 100) : 0L;
    }

    public static boolean isCompleteAllMold(Step step) {
        switch (step.getReportType()) {
            case BY_MOLD:
                return step.getListMoldProgress().stream()
                        .allMatch(MoldProgress::getIsCompleted);
            case BY_MOLD_ELEMENT:
                return step.getListMoldGroupElementProgresses().stream()
                        .allMatch(MoldGroupElementProgress::getIsCompleted);
            case BY_MOLD_SEND_RECEIVE:
                return step.getListMoldDeliverProgress().stream()
                        .allMatch(MoldDeliverProgress::getIsCompleted);
            default:
                return false;
        }
    }
}
