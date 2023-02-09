package com.shop.tbms.component;

import com.shop.tbms.constant.StepConstant;
import com.shop.tbms.dto.mold.MoldGroupDetailReqDTO;
import com.shop.tbms.dto.mold.MoldGroupReqDTO;
import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.mold.MoldType;
import com.shop.tbms.enumerate.step.StepStatus;
import com.shop.tbms.enumerate.step.StepType;
import com.shop.tbms.repository.MoldDeliverProgressRepository;
import com.shop.tbms.repository.MoldGroupElementProgressRepository;
import com.shop.tbms.repository.MoldProgressRepository;
import com.shop.tbms.repository.StepRepository;
import com.shop.tbms.util.ProgressUtil;
import com.shop.tbms.util.StepConditionUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Slf4j
public class ProgressMoldGroupComponent {
    @Autowired
    private StepConstant stepConstant;

    @Autowired
    private MoldProgressRepository moldProgressRepository;
    @Autowired
    private MoldGroupElementProgressRepository moldGroupElementProgressRepository;
    @Autowired
    private MoldDeliverProgressRepository moldDeliverProgressRepository;
    @Autowired
    private StepRepository stepRepository;

    public void generateProgressForCreateGroup(MoldGroupReqDTO reqDTO, PurchaseOrder order, List<Mold> listUpdateMold) {
        List<MoldProgress> listUpdatedMoldProgress = new ArrayList<>();
        List<MoldDeliverProgress> listUpdatedMoldDeliverProgress = new ArrayList<>();
        List<MoldGroupElementProgress> listUpdatedMoldElementProgress = new ArrayList<>();

        boolean isFreeFormType = MoldType.FREEFORM.equals(reqDTO.getMoldGroup().getType());

        for (Step step : order.getProcedure().getListStep()) {
            if (stepConstant.getCodePHONG_DIEN().equalsIgnoreCase(step.getCode())) {
                /* gen step Phong dien */
                if (reqDTO.getMoldGroup().isHasBanDien() || isFreeFormType) {
                    List<MoldProgress> moldProgressListForConditionStep = ProgressUtil.generateMoldProcessForMoldGroup(
                            step,
                            listUpdateMold);

                    step.setListMoldProgress(moldProgressListForConditionStep);

                    listUpdatedMoldProgress.addAll(moldProgressListForConditionStep);
                }
            }

            if ((!isFreeFormType || (!stepConstant.getListStepNotForFreeFormType().contains(step.getCode()))) && !StepType.FIXING.equals(step.getType())) {
                switch (step.getReportType()) {
                    case BY_MOLD:
                        List<MoldProgress> moldProgressListForMoldGroup = ProgressUtil.generateMoldProcessForMoldGroup(
                                step,
                                listUpdateMold);

                        step.setListMoldProgress(moldProgressListForMoldGroup);

                        listUpdatedMoldProgress.addAll(moldProgressListForMoldGroup);
                        break;
                    case BY_MOLD_SEND_RECEIVE:
                        List<MoldDeliverProgress> moldDeliverProgressListForMoldGroup = ProgressUtil.generateMoldDeliverProcessForMoldGroup(
                                step,
                                listUpdateMold);

                        step.setListMoldDeliverProgress(moldDeliverProgressListForMoldGroup);

                        listUpdatedMoldDeliverProgress.addAll(moldDeliverProgressListForMoldGroup);
                        break;
                    case BY_MOLD_ELEMENT:
                        List<MoldGroupElementProgress> moldGroupElementProgressList = ProgressUtil.generateMoldGroupElementProgress(
                                step,
                                listUpdateMold);

                        step.setListMoldGroupElementProgresses(moldGroupElementProgressList);

                        listUpdatedMoldElementProgress.addAll(moldGroupElementProgressList);
                        break;
                    default:
                }
            }
        }

        moldProgressRepository.saveAll(listUpdatedMoldProgress);
        moldDeliverProgressRepository.saveAll(listUpdatedMoldDeliverProgress);
        moldGroupElementProgressRepository.saveAll(listUpdatedMoldElementProgress);
    }

    public void resetMoldGroupProgressChangeType(Mold mold, MoldGroupDetailReqDTO detailReqDTO) {
        PurchaseOrder order = mold.getPurchaseOrder();
        String resetToStepCode = stepConstant.getStepToResetToStepWhenChangeMoldDetail(
                Objects.nonNull(mold.getMoldGroup()) ? mold.getMoldGroup().getType() : null,
                detailReqDTO.getType()
        );

        /* get step need to reset when mold group type changed */
        Step resetToStep = stepRepository.findFirstByCodeAndProcedureOrderId(resetToStepCode, order.getId()).orElseThrow();

        /* get all step after reset step */
        List<Step> listStepNeedToReset = order.getProcedure().getListStep().stream()
                .filter(step ->
                        step.getSequenceNo().compareTo(resetToStep.getSequenceNo()) >= 0
                )
                .collect(Collectors.toList());

        /* reset progress */
        listStepNeedToReset.forEach(step -> this.resetProgressChangeType(step, List.of(mold), detailReqDTO));
    }

    private void resetProgressChangeType(Step step, List<Mold> listMold, MoldGroupDetailReqDTO detailReqDTO) {
        log.info("Begin reset progress mold {} of step {}", listMold, step);

        /* set status to IN PROGRESS */
        if (!StepStatus.IN_PROGRESS.equals(step.getStatus())) {
            step.setStatus(StepStatus.INIT.equals(step.getStatus()) ? StepStatus.INIT : StepStatus.IN_PROGRESS);
        }

        switch (step.getReportType()) {
            case BY_MOLD:
                resetMoldProgressChangeType(step, listMold, detailReqDTO);
                break;
            case BY_MOLD_ELEMENT:
                resetMoldGroupProgressChangeType(step, listMold, detailReqDTO);
                break;
            case BY_MOLD_SEND_RECEIVE:
                resetMoldDeliverProgressChangeType(step, listMold, detailReqDTO);
                break;
        }
    }

    private void resetMoldProgressChangeType(Step step, List<Mold> listMold, MoldGroupDetailReqDTO detailReqDTO) {
        List<MoldProgress> moldProgressList = moldProgressRepository.findAllByStepId(step.getId());
        log.info("get list progress list of step {} get result {}", step, moldProgressList);
        List<MoldProgress> listUpdatedProgress = new ArrayList<>();
        List<MoldProgress> listDeletedProgress = new ArrayList<>();

        listMold.forEach(mold -> {
            log.info("Start change complete mold {} of step {}", mold, step);
            Optional<MoldProgress> optionalMoldProgress = moldProgressList.stream()
                    .filter(moldProgress -> moldProgress.getMold().getId().equals(mold.getId()))
                    .findFirst();

            if (StepType.FIXING.equals(step.getType())) {
                /* if Step is fixing step, delete progress */
                if (optionalMoldProgress.isPresent()) {
                    MoldProgress moldProgress = optionalMoldProgress.get();
                    log.info("mold progress is exited {}. Delete progress", moldProgress);
                    listDeletedProgress.add(moldProgress);
                }
            } else if (StepConditionUtil.isStepHasConditionProgress(step, stepConstant)) {
                boolean isReqTypeFreeForm = MoldType.FREEFORM.equals(detailReqDTO.getType());
                boolean isCurTypeFreeForm = Objects.nonNull(mold.getMoldGroup()) && MoldType.FREEFORM.equals(mold.getMoldGroup().getType());

                if (isReqTypeFreeForm && !isCurTypeFreeForm) {
                    /* update from other type to type FREEFORM */
                    /* delete progress */
                    if (optionalMoldProgress.isPresent()) {
                        MoldProgress moldProgress = optionalMoldProgress.get();
                        log.info("mold progress is exited {}. Delete progress", moldProgress);
                        listDeletedProgress.add(moldProgress);
                    }
                } else if (!isReqTypeFreeForm && isCurTypeFreeForm) {
                    /* update from type FREEFORM to other TYPE */
                    /* add new progress */
                    List<MoldProgress> moldProgressListForConditionStep = ProgressUtil.generateMoldProcessForMoldGroup(
                            step,
                            List.of(mold));

                    step.setListMoldProgress(moldProgressListForConditionStep);

                    listUpdatedProgress.addAll(moldProgressListForConditionStep);
                } else {
                    /* check hasBanDien */
                    if (!isReqTypeFreeForm && stepConstant.getCodePHONG_DIEN().equalsIgnoreCase(step.getCode())) {
                        /* check option Ban Dien only for type other than FREEFORM */
                        boolean isReqHasBanDien = detailReqDTO.isHasBanDien();
                        boolean isCurHasBanDien = Objects.nonNull(mold.getMoldGroup()) && mold.getMoldGroup().getHasBanDien();

                        if (!isCurHasBanDien && isReqHasBanDien) {
                            /* Check to has Ban Dien */
                            /* Generate progress for step Phong Dien */
                            log.info("Check hasBanDien, create progress for mold {} at step PHONG DIEN {}", mold, step);

                            List<MoldProgress> moldProgressListForConditionStep = ProgressUtil.generateMoldProcessForMoldGroup(
                                    step,
                                    List.of(mold));

                            step.setListMoldProgress(moldProgressListForConditionStep);

                            listUpdatedProgress.addAll(moldProgressListForConditionStep);
                        } else if (isCurHasBanDien && !isReqHasBanDien) {
                            /* Uncheck to has Ban Dien */
                            /* Delete progress for step Phong Dien */
                            log.info("Uncheck hasBanDien, delete progress for mold {} at step PHONG DIEN {}", mold, step);

                            List<MoldProgress> listProgressNeedRemoved = step.getListMoldProgress().stream()
                                    .filter(moldProgress ->
                                            mold.getSize().equalsIgnoreCase(moldProgress.getMold().getSize()))
                                    .collect(Collectors.toList());
                            step.getListMoldProgress().removeAll(listProgressNeedRemoved);

                            log.info("Delete progress {}", listProgressNeedRemoved);
                            listDeletedProgress.addAll(listProgressNeedRemoved);
                        } else {
                            /* reset to not complete */
                            if (optionalMoldProgress.isPresent()) {
                                MoldProgress moldProgress = optionalMoldProgress.get();
                                log.info("mold progress is exited {}. Change complete to FALSE", moldProgress);
                                moldProgress.setIsCompleted(Boolean.FALSE);

                                listUpdatedProgress.add(moldProgress);
                            }
                        }
                    } else {
                        /* not step Phong Dien || type not FREEFORM */
                        if (optionalMoldProgress.isPresent()) {
                            MoldProgress moldProgress = optionalMoldProgress.get();
                            log.info("mold progress is exited {}. Change complete to FALSE", moldProgress);
                            moldProgress.setIsCompleted(Boolean.FALSE);

                            listUpdatedProgress.add(moldProgress);
                        }
                    }
                }
            } else {
                /* else set progress complete to false */
                if (optionalMoldProgress.isPresent()) {
                    MoldProgress moldProgress = optionalMoldProgress.get();
                    log.info("mold progress is exited {}. Change complete to FALSE", moldProgress);
                    moldProgress.setIsCompleted(Boolean.FALSE);

                    listUpdatedProgress.add(moldProgress);
                }
            }
        });

        log.info("Updated progress {}", listUpdatedProgress);
        log.info("Deleted progress {}", listDeletedProgress);

        moldProgressRepository.deleteAll(listDeletedProgress);
        moldProgressRepository.saveAll(listUpdatedProgress);
        log.info("End reset mold progress.");
    }

    private void resetMoldGroupProgressChangeType(Step step, List<Mold> listMold, MoldGroupDetailReqDTO detailReqDTO) {
        List<MoldGroupElementProgress> listUpdatedProgress = new ArrayList<>();

        listMold.forEach(mold -> {
            log.info("Start delete progress mold {} of step {}", mold, step);
            moldGroupElementProgressRepository.removeByStepIdAndMoldId(step.getId(), mold.getId());

            if (!MoldType.FREEFORM.equals(detailReqDTO.getType()) || !StepConditionUtil.isStepHasConditionProgress(step, stepConstant)) {
                listUpdatedProgress.addAll(ProgressUtil.generateMoldGroupElementProgress(step, List.of(mold)));
            }
        });

        log.info("Updated progress {}", listUpdatedProgress);
        moldGroupElementProgressRepository.saveAll(listUpdatedProgress);
        log.info("End reset mold progress.");
    }

    private void resetMoldDeliverProgressChangeType(Step step, List<Mold> listMold, MoldGroupDetailReqDTO detailReqDTO) {
        List<MoldDeliverProgress> progressList = moldDeliverProgressRepository.findAllByStepId(step.getId());
        log.info("get list progress list of step {} get result {}", step, progressList);
        List<MoldDeliverProgress> listUpdatedProgress = new ArrayList<>();
        List<MoldDeliverProgress> listDeletedProgress = new ArrayList<>();

        listMold.forEach(mold -> {
            log.info("Start change complete mold {} of step {}", mold, step);
            List<MoldDeliverProgress> deliverProgressList = progressList.stream()
                    .filter(moldProgress -> moldProgress.getMold().getId().equals(mold.getId()))
                    .collect(Collectors.toList());

            if (StepType.FIXING.equals(step.getType())) {
                /* if Step is fixing step, delete progress */
                listDeletedProgress.addAll(deliverProgressList);
            } else if (StepConditionUtil.isStepHasConditionProgress(step, stepConstant)) {
                boolean isReqTypeFreeForm = MoldType.FREEFORM.equals(detailReqDTO.getType());
                boolean isCurTypeFreeForm = Objects.nonNull(mold.getMoldGroup()) && MoldType.FREEFORM.equals(mold.getMoldGroup().getType());

                if (isReqTypeFreeForm && !isCurTypeFreeForm) {
                    /* update from other type to type FREEFORM */
                    /* delete progress */
                    listDeletedProgress.addAll(deliverProgressList);
                } else if (!isReqTypeFreeForm && isCurTypeFreeForm) {
                    /* update from type FREEFORM to other TYPE */
                    /* add new progress */
                    List<MoldDeliverProgress> moldDeliverProgressListForConditionStep = ProgressUtil.generateMoldDeliverProcessForMoldGroup(
                            step,
                            List.of(mold));

                    step.setListMoldDeliverProgress(moldDeliverProgressListForConditionStep);

                    listUpdatedProgress.addAll(moldDeliverProgressListForConditionStep);
                } else {
                    deliverProgressList.forEach(progress -> {
                        log.info("deliver progress is exited {}. Change complete to FALSE", progress);
                        progress.setIsCompleted(Boolean.FALSE);

                        listUpdatedProgress.add(progress);
                    });
                }
            } else {
                deliverProgressList.forEach(progress -> {
                    log.info("deliver progress is exited {}. Change complete to FALSE", progress);
                    progress.setIsCompleted(Boolean.FALSE);

                    listUpdatedProgress.add(progress);
                });
            }
        });

        log.info("Updated progress {}", listUpdatedProgress);
        log.info("Deleted progress {}", listDeletedProgress);

        moldDeliverProgressRepository.saveAll(listUpdatedProgress);
        moldDeliverProgressRepository.deleteAll(listDeletedProgress);
        log.info("End reset deliver progress.");
    }
}
