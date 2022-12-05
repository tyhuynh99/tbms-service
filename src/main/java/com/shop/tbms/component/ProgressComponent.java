package com.shop.tbms.component;

import com.shop.tbms.constant.StepConstant;
import com.shop.tbms.dto.log.ReportProgressCompleteDTO;
import com.shop.tbms.dto.step.detail.progress.MoldDeliverProgressDTO;
import com.shop.tbms.dto.step.detail.progress.MoldElementProgressDTO;
import com.shop.tbms.dto.step.detail.progress.MoldElementProgressDetailDTO;
import com.shop.tbms.dto.step.detail.progress.MoldProgressDTO;
import com.shop.tbms.entity.*;
import com.shop.tbms.entity.common.AbstractAuditingEntity;
import com.shop.tbms.enumerate.mold.MoldDeliverProgressType;
import com.shop.tbms.enumerate.step.ReportType;
import com.shop.tbms.enumerate.step.StepStatus;
import com.shop.tbms.repository.MoldDeliverProgressRepository;
import com.shop.tbms.repository.MoldGroupElementProgressRepository;
import com.shop.tbms.repository.MoldProgressRepository;
import com.shop.tbms.repository.StepRepository;
import com.shop.tbms.util.MoldUtil;
import com.shop.tbms.util.ProgressUtil;
import com.shop.tbms.util.StepUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

@Component
@Transactional
@Slf4j
public class ProgressComponent {
    @Autowired
    private MoldProgressRepository moldProgressRepository;
    @Autowired
    private MoldDeliverProgressRepository moldDeliverProgressRepository;
    @Autowired
    private MoldGroupElementProgressRepository moldGroupElementProgressRepository;
    @Autowired
    private StepRepository stepRepository;

    @Autowired
    private StepConstant stepConstant;

    public void generateProgressForStep(Step step) {
        log.info("Start generate progress for step {}", step);
        switch (step.getReportType()) {
            case BY_MOLD:
                moldProgressRepository.saveAll(ProgressUtil.generateMoldProcess(step));
                break;
            case BY_MOLD_SEND_RECEIVE:
                moldDeliverProgressRepository.saveAll(ProgressUtil.generateMoldDeliverProcess(step));
                break;
            case BY_MOLD_ELEMENT:
                moldGroupElementProgressRepository.saveAll(ProgressUtil.generateMoldGroupElementProgress(step));
                break;
            default:
        }
        log.info("End generate progress for step {}", step);
    }

    public List<MoldProgressDTO> setReportAvailabilityForMoldProgress(Step preStep, Step nextStep, List<MoldProgressDTO> moldProgressDTOList) {
        return moldProgressDTOList.stream().map(moldProgressDTO -> {
            boolean canCheck = true;
            if (Objects.nonNull(preStep)) {
                canCheck = canCheckCompleteBySize(preStep, moldProgressDTO.getMoldSize());
            }
            log.info("Set value canCheck of {} is {}", moldProgressDTO, canCheck);
            moldProgressDTO.setCanCheck(canCheck);

            boolean canUncheck = true;
            if (Objects.nonNull(nextStep)) {
                canUncheck = canUnCheckCompleteBySize(nextStep, moldProgressDTO.getMoldSize());
            }
            log.info("Set value canUncheck of {} is {}", moldProgressDTO, canUncheck);
            moldProgressDTO.setCanUncheck(canUncheck);

            return moldProgressDTO;
        }).collect(Collectors.toList());
    }

    public List<MoldElementProgressDTO> setReportAvailabilityForMoldElementProgress(Step preStep, Step nextStep, List<MoldElementProgressDTO> moldElementProgressDTOList) {
        return moldElementProgressDTOList.stream().map(moldElementProgressDTO -> {
            boolean canCheck = true;
            if (Objects.nonNull(preStep)) {
                canCheck = canCheckCompleteBySize(preStep, moldElementProgressDTO.getMoldSize());
            }
            final boolean canCheckFinal = canCheck;
            log.info("Set value canCheck of {} is {}", moldElementProgressDTO, canCheck);

            boolean canUncheck = true;
            if (Objects.nonNull(nextStep)) {
                canUncheck = canUnCheckCompleteBySize(nextStep, moldElementProgressDTO.getMoldSize());
            }
            final boolean canUncheckFinal = canUncheck;
            log.info("Set value canUncheck of {} is {}", moldElementProgressDTO, canUncheck);

            List<MoldElementProgressDetailDTO> moldElementProgressDetailDTOList = moldElementProgressDTO.getListElement().stream().map(moldElementProgressDetailDTO -> {
                moldElementProgressDetailDTO.setCanCheck(canCheckFinal);
                moldElementProgressDetailDTO.setCanUncheck(canUncheckFinal);
                return moldElementProgressDetailDTO;
            }).collect(Collectors.toList());

            moldElementProgressDTO.setListElement(moldElementProgressDetailDTOList);

            return moldElementProgressDTO;
        }).collect(Collectors.toList());
    }

    public List<MoldDeliverProgressDTO> setReportAvailabilityForDeliveryProgress(Step preStep, Step nextStep, List<MoldDeliverProgressDTO> moldDeliverProgressDTOList) {
        return moldDeliverProgressDTOList.stream().map(moldDeliverProgressDTO -> {
            boolean canCheck = true;
            if (Objects.nonNull(preStep)) {
                canCheck = canCheckCompleteBySize(preStep, moldDeliverProgressDTO.getMoldSize());
            }
            log.info("Set value canCheck of {} is {}", moldDeliverProgressDTO, canCheck);
            moldDeliverProgressDTO.setCanCheck(canCheck);

            boolean canUncheck = true;
            if (Objects.nonNull(nextStep)) {
                canUncheck = canUnCheckCompleteBySize(nextStep, moldDeliverProgressDTO.getMoldSize());
            }
            log.info("Set value canUncheck of {} is {}", moldDeliverProgressDTO, canUncheck);
            moldDeliverProgressDTO.setCanUncheck(canUncheck);

            return moldDeliverProgressDTO;
        }).collect(Collectors.toList());
    }

    public boolean canCheckCompleteBySize(Step preStep, String moldSize) {
        switch (preStep.getReportType()) {
            case BY_MOLD:
                log.info("Check complete of mold size {} with preStep mold progress {}", moldSize, preStep.getListMoldProgress());
                List<MoldProgress> progressList = preStep.getListMoldProgress().stream()
                        .filter(moldProgress -> moldSize.equals(moldProgress.getMold().getSize()))
                        .collect(Collectors.toList());

                if (CollectionUtils.isEmpty(progressList)) {
                    if (Boolean.TRUE.equals(preStep.getHasCondition())) {
                        log.info("Step hasCondition, and not found progress of mold size {}. Need to check preStep", moldSize);
                        Step preOfPreStep = StepUtil.getPreMainStep(preStep.getListStepAfter());
                        return canCheckCompleteBySize(preOfPreStep, moldSize);
                    }
                }
                return progressList.stream().allMatch(moldProgress -> Boolean.TRUE.equals(moldProgress.getIsCompleted()));
            case BY_MOLD_ELEMENT:
                log.info("Check complete of mold size {} with preStep mold element progress {}", moldSize, preStep.getListMoldGroupElementProgresses());
                List<MoldGroupElementProgress> elementProgressesBySize = preStep.getListMoldGroupElementProgresses().stream()
                        .filter(moldGroupElementProgress ->
                                moldSize.equals(moldGroupElementProgress.getMold().getSize()))
                        .collect(Collectors.toList());

                if (CollectionUtils.isEmpty(elementProgressesBySize)) {
                    if (Boolean.TRUE.equals(preStep.getHasCondition())) {
                        log.info("Step hasCondition, and not found progress of mold size {}. Need to check preStep", moldSize);
                        Step preOfPreStep = StepUtil.getPreMainStep(preStep.getListStepAfter());
                        return canCheckCompleteBySize(preOfPreStep, moldSize);
                    }
                    return false;
                }

                return elementProgressesBySize.stream()
                        .allMatch(MoldGroupElementProgress::getIsCompleted);
            case BY_MOLD_SEND_RECEIVE:
                log.info("Check complete of mold size {} with preStep mold deliver progress {}", moldSize, preStep.getListMoldDeliverProgress());
                List<MoldDeliverProgress> deliverProgressList = preStep.getListMoldDeliverProgress().stream()
                        .filter(moldProgress -> moldSize.equals(moldProgress.getMold().getSize()))
                        .collect(Collectors.toList());

                if (CollectionUtils.isEmpty(deliverProgressList)) {
                    if (Boolean.TRUE.equals(preStep.getHasCondition())) {
                        log.info("Step hasCondition, and not found progress of mold size {}. Need to check preStep", moldSize);
                        Step preOfPreStep = StepUtil.getPreMainStep(preStep.getListStepAfter());
                        return canCheckCompleteBySize(preOfPreStep, moldSize);
                    }
                }

                return deliverProgressList.stream().allMatch(MoldDeliverProgress::getIsCompleted);
            default:
                return false;
        }
    }

    public boolean canUnCheckCompleteBySize(Step nextStep, String moldSize) {
        switch (nextStep.getReportType()) {
            case BY_MOLD:
                log.info("Check complete of mold size {} with nextStep mold progress {}", moldSize, nextStep.getListMoldProgress());
                List<MoldProgress> progressList = nextStep.getListMoldProgress().stream()
                        .filter(moldProgress -> moldSize.equals(moldProgress.getMold().getSize()))
                        .collect(Collectors.toList());

                if (CollectionUtils.isEmpty(progressList)) {
                    if (Boolean.TRUE.equals(nextStep.getHasCondition())) {
                        log.info("Step hasCondition, and not found progress of mold size {}. Need to check nextStep", moldSize);
                        Step nextOfNextStep = StepUtil.getNextMainStep(nextStep.getListStepAfter());
                        return canUnCheckCompleteBySize(nextOfNextStep, moldSize);
                    }
                }

                return progressList.stream().allMatch(moldProgress -> Boolean.FALSE.equals(moldProgress.getIsCompleted()));
            case BY_MOLD_ELEMENT:
                log.info("Check complete of mold size {} with nextStep mold element progress {}", moldSize, nextStep.getListMoldGroupElementProgresses());
                List<MoldGroupElementProgress> elementProgressList = nextStep.getListMoldGroupElementProgresses().stream()
                        .filter(moldGroupElementProgress ->
                                moldSize.equals(moldGroupElementProgress.getMold().getSize()))
                        .collect(Collectors.toList());

                if (CollectionUtils.isEmpty(elementProgressList)) {
                    if (Boolean.TRUE.equals(nextStep.getHasCondition())) {
                        log.info("Step hasCondition, and not found progress of mold size {}. Need to check nextStep", moldSize);
                        Step nextOfNextStep = StepUtil.getNextMainStep(nextStep.getListStepAfter());
                        return canUnCheckCompleteBySize(nextOfNextStep, moldSize);
                    }
                }

                return elementProgressList.stream().allMatch(Predicate.not(MoldGroupElementProgress::getIsCompleted));
            case BY_MOLD_SEND_RECEIVE:
                log.info("Check complete of mold size {} with nextStep mold deliver progress {}", moldSize, nextStep.getListMoldDeliverProgress());
                List<MoldDeliverProgress> deliverProgressList = nextStep.getListMoldDeliverProgress().stream()
                        .filter(moldDeliverProgress ->
                                moldSize.equals(moldDeliverProgress.getMold().getSize()))
                        .collect(Collectors.toList());

                if (CollectionUtils.isEmpty(deliverProgressList)) {
                    if (Boolean.TRUE.equals(nextStep.getHasCondition())) {
                        log.info("Step hasCondition, and not found progress of mold size {}. Need to check nextStep", moldSize);
                        Step nextOfNextStep = StepUtil.getNextMainStep(nextStep.getListStepAfter());
                        return canUnCheckCompleteBySize(nextOfNextStep, moldSize);
                    }
                }

                return deliverProgressList.stream().allMatch(Predicate.not(MoldDeliverProgress::getIsCompleted));
            default:
                return false;
        }
    }

    public void resetProgress(Step step, List<Mold> listMold) {
        log.info("Begin reset progress mold {} of step {}", listMold, step);

        /* set status to IN PROGRESS */
        if (!StepStatus.IN_PROGRESS.equals(step.getStatus())) {
            step.setStatus(StepStatus.INIT.equals(step.getStatus()) ? StepStatus.INIT : StepStatus.IN_PROGRESS);
        }

        switch (step.getReportType()) {
            case BY_MOLD:
                resetMoldProgress(step, listMold);
                break;
            case BY_MOLD_ELEMENT:
                resetMoldGroupProgress(step, listMold);
                break;
            case BY_MOLD_SEND_RECEIVE:
                resetMoldDeliverProgress(step, listMold);
                break;
        }
    }

    private void resetMoldProgress(Step step, List<Mold> listMold) {
        List<MoldProgress> moldProgressList = moldProgressRepository.findAllByStepId(step.getId());
        log.info("get list progress list of step {} get result {}", step, moldProgressList);
        List<MoldProgress> listUpdatedProgress = new ArrayList<>();

        listMold.forEach(mold -> {
            log.info("Start change complete mold {} of step {}", mold, step);
            Optional<MoldProgress> optionalMoldProgress = moldProgressList.stream()
                    .filter(moldProgress -> moldProgress.getMold().getId().equals(mold.getId()))
                    .findFirst();

            if (optionalMoldProgress.isPresent()) {
                MoldProgress moldProgress = optionalMoldProgress.get();
                log.info("mold progress is exited {}. Change complete to FALSE", moldProgress);
                moldProgress.setIsCompleted(Boolean.FALSE);

                listUpdatedProgress.add(moldProgress);
            } else {
                log.info("mold progress is not existed. create new mold progress.");
                MoldProgress moldProgress = new MoldProgress();
                moldProgress.setMold(mold);
                moldProgress.setStep(step);
                moldProgress.setIsCompleted(Boolean.FALSE);

                listUpdatedProgress.add(moldProgress);
            }
        });

        log.info("Updated progress {}", listUpdatedProgress);
        moldProgressRepository.saveAll(listUpdatedProgress);
        log.info("End reset mold progress.");
    }

    private void resetMoldGroupProgress(Step step, List<Mold> listMold) {
        List<MoldGroupElementProgress> listUpdatedProgress = new ArrayList<>();

        listMold.forEach(mold -> {
            log.info("Start delete progress mold {} of step {}", mold, step);
            moldGroupElementProgressRepository.removeByStepIdAndMoldId(step.getId(), mold.getId());

            listUpdatedProgress.addAll(ProgressUtil.generateMoldGroupElementProgress(step, mold));
        });

        log.info("Updated progress {}", listUpdatedProgress);
        moldGroupElementProgressRepository.saveAll(listUpdatedProgress);
        log.info("End reset mold progress.");
    }

    private void resetMoldDeliverProgress(Step step, List<Mold> listMold) {
        List<MoldDeliverProgress> progressList = moldDeliverProgressRepository.findAllByStepId(step.getId());
        log.info("get list progress list of step {} get result {}", step, progressList);
        List<MoldDeliverProgress> listUpdatedProgress = new ArrayList<>();

        listMold.forEach(mold -> {
            log.info("Start change complete mold {} of step {}", mold, step);
            List<MoldDeliverProgress> deliverProgressList = progressList.stream()
                    .filter(moldProgress -> moldProgress.getMold().getId().equals(mold.getId()))
                    .collect(Collectors.toList());

            if (deliverProgressList.isEmpty()) {
                log.info("deliver progress is not existed. create new delvier progress.");
                MoldDeliverProgress sendProgress = new MoldDeliverProgress();
                sendProgress.setMold(mold);
                sendProgress.setStep(step);
                sendProgress.setType(MoldDeliverProgressType.SEND);
                sendProgress.setIsCompleted(Boolean.FALSE);

                listUpdatedProgress.add(sendProgress);

                MoldDeliverProgress receiveProgress = new MoldDeliverProgress();
                receiveProgress.setMold(mold);
                receiveProgress.setStep(step);
                receiveProgress.setType(MoldDeliverProgressType.RECEIVE);
                receiveProgress.setIsCompleted(Boolean.FALSE);

                listUpdatedProgress.add(receiveProgress);
            } else {
                deliverProgressList.forEach(progress -> {
                    log.info("deliver progress is exited {}. Change complete to FALSE", progress);
                    progress.setIsCompleted(Boolean.FALSE);

                    listUpdatedProgress.add(progress);
                });
            }
        });

        log.info("Updated progress {}", listUpdatedProgress);
        moldDeliverProgressRepository.saveAll(listUpdatedProgress);
        log.info("End reset deliver progress.");
    }

    public void resetMoldGroupProgressChangeType(Mold mold) {
        PurchaseOrder order = mold.getPurchaseOrder();
        String resetToStepCode = stepConstant.getStepToResetToStepWhenChangeMoldDetail();

        /* get step need to reset when mold group type changed */
        Step resetToStep = stepRepository.findFirstByCodeAndProcedureOrderId(resetToStepCode, order.getId()).orElseThrow();

        /* get all step after reset step */
        List<Step> listStepNeedToReset = order.getProcedure().getListStep().stream()
                .filter(step ->
                        step.getSequenceNo().compareTo(resetToStep.getSequenceNo()) >= 0
                )
                .collect(Collectors.toList());

        /* reset progress */
        listStepNeedToReset.forEach(step -> this.resetProgress(step, List.of(mold)));
    }

    public void resetMoldGroupProgressChangeElement(Mold mold) {
        PurchaseOrder order = mold.getPurchaseOrder();
        String resetToStepCode = stepConstant.getStepToResetToStepWhenChangeMoldDetail();

        /* get step need to reset when mold group type changed */
        Step resetToStep = stepRepository.findFirstByCodeAndProcedureOrderId(resetToStepCode, order.getId()).orElseThrow();

        /* get all step after reset step */
        List<Step> listStepNeedToReset = order.getProcedure().getListStep().stream()
                .filter(step ->
                        step.getSequenceNo().compareTo(resetToStep.getSequenceNo()) >= 0
                )
                .collect(Collectors.toList());

        /* reset progress */
        listStepNeedToReset.forEach(step -> this.resetMoldGroupProgressChangeElement(mold, step));
    }

    public void resetMoldGroupProgressChangeElement(Mold mold, Step step) {
        if (ReportType.BY_MOLD_ELEMENT.equals(step.getReportType()) && StepStatus.IN_PROGRESS.equals(step.getStatus())) {
            List<MoldGroupElement> elementList = mold.getMoldGroup().getListMoldGroupElement();
            List<MoldGroupElementProgress> listProgress = moldGroupElementProgressRepository.findAllByStepId(step.getId());

            List<MoldGroupElementProgress> deletedProgress = new ArrayList<>();
            List<MoldGroupElementProgress> addedProgress = new ArrayList<>();

            for (MoldGroupElement element : elementList) {
                Optional<MoldGroupElementProgress> chkProgress = listProgress.stream()
                        .filter(moldGroupElementProgress ->
                                element.getName().equalsIgnoreCase(
                                        moldGroupElementProgress.getMoldGroupElement().getName()))
                        .findFirst();

                if (chkProgress.isPresent()) {
                    if (Boolean.FALSE.equals(element.getChecked())) {
                        deletedProgress.add(chkProgress.get());
                    }
                } else if (Boolean.TRUE.equals(element.getChecked())) {
                    MoldGroupElementProgress progress = new MoldGroupElementProgress();
                    progress.setMold(mold);
                    progress.setStep(step);
                    progress.setMoldGroupElement(element);
                    progress.setIsCompleted(Boolean.FALSE);

                    addedProgress.add(progress);
                }
            }

            listProgress.removeAll(deletedProgress);
            listProgress.addAll(addedProgress);

            moldGroupElementProgressRepository.saveAll(listProgress);
        }
    }

    public List<ReportProgressCompleteDTO> getListCompleteReport(Step step) {
        switch (step.getReportType()) {
            case BY_MOLD:
                return getListCompleteReportByMold(step);
            case BY_MOLD_ELEMENT:
                return getListCompleteReportByElement(step);
            case BY_MOLD_SEND_RECEIVE:
                return getListCompleteReportByDeliver(step);
            default:
                return List.of();
        }
    }

    public List<ReportProgressCompleteDTO> getListCompleteReportByMold(Step step) {
        List<MoldProgress> progressList = step.getListMoldProgress();

        List<ReportProgressCompleteDTO> dtoList = new ArrayList<>();
        progressList.forEach(progress -> {
            if (Boolean.TRUE.equals(progress.getIsCompleted())) {
                dtoList.add(
                        ReportProgressCompleteDTO
                                .builder()
                                .completeAt(progress.getUpdatedDate().toLocalDate())
                                .mold(MoldUtil.getMoldName(progress.getMold()))
                                .build()
                );
            }
        });

        return dtoList;
    }

    public List<ReportProgressCompleteDTO> getListCompleteReportByDeliver(Step step) {
        List<MoldDeliverProgress> progressList = step.getListMoldDeliverProgress();
        Map<Mold, List<MoldDeliverProgress>> mapProgressByMold = new HashMap<>();

        progressList.forEach(moldDeliverProgresses -> {
            Mold mold = moldDeliverProgresses.getMold();
            if (mapProgressByMold.containsKey(mold)) {
                mapProgressByMold.get(mold).add(moldDeliverProgresses);
            } else {
                List<MoldDeliverProgress> progressListByMold = Arrays.asList(moldDeliverProgresses);
                mapProgressByMold.put(mold, progressListByMold);
            }
        });

        List<ReportProgressCompleteDTO> dtoList = new ArrayList<>();

        mapProgressByMold.keySet().forEach(mold -> {
            List<MoldDeliverProgress> progressListByMold = mapProgressByMold.get(mold);

            boolean isMoldComplete = progressListByMold.stream()
                    .map(MoldDeliverProgress::getIsCompleted)
                    .allMatch(Predicate.isEqual(Boolean.TRUE));

            if (isMoldComplete) {
                LocalDate completedDay = progressListByMold.stream()
                        .map(AbstractAuditingEntity::getUpdatedDate)
                        .max(LocalDateTime::compareTo)
                        .orElseThrow()
                        .toLocalDate();

                dtoList.add(
                        ReportProgressCompleteDTO
                                .builder()
                                .completeAt(completedDay)
                                .mold(MoldUtil.getMoldName(mold))
                                .build()
                );
            }
        });

        return dtoList;
    }

    public List<ReportProgressCompleteDTO> getListCompleteReportByElement(Step step) {
        List<MoldGroupElementProgress> progressList = step.getListMoldGroupElementProgresses();
        Map<Mold, List<MoldGroupElementProgress>> mapProgressByMold = new HashMap<>();

        progressList.forEach(progress -> {
            Mold mold = progress.getMold();
            if (mapProgressByMold.containsKey(mold)) {
                mapProgressByMold.get(mold).add(progress);
            } else {
                List<MoldGroupElementProgress> progressListByMold = Arrays.asList(progress);
                mapProgressByMold.put(mold, progressListByMold);
            }
        });

        List<ReportProgressCompleteDTO> dtoList = new ArrayList<>();

        mapProgressByMold.keySet().forEach(mold -> {
            List<MoldGroupElementProgress> progressListByMold = mapProgressByMold.get(mold);

            boolean isMoldComplete = progressListByMold.stream()
                    .map(MoldGroupElementProgress::getIsCompleted)
                    .allMatch(Predicate.isEqual(Boolean.TRUE));

            if (isMoldComplete) {
                LocalDate completedDay = progressListByMold.stream()
                        .map(AbstractAuditingEntity::getUpdatedDate)
                        .max(LocalDateTime::compareTo)
                        .orElseThrow()
                        .toLocalDate();

                dtoList.add(
                        ReportProgressCompleteDTO
                                .builder()
                                .completeAt(completedDay)
                                .mold(MoldUtil.getMoldName(mold))
                                .build()
                );
            }
        });

        return dtoList;
    }
}
