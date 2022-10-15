package com.shop.tbms.component;

import com.shop.tbms.constant.StepConstant;
import com.shop.tbms.dto.step.detail.progress.MoldDeliverProgressDTO;
import com.shop.tbms.dto.step.detail.progress.MoldElementProgressDTO;
import com.shop.tbms.dto.step.detail.progress.MoldElementProgressDetailDTO;
import com.shop.tbms.dto.step.detail.progress.MoldProgressDTO;
import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.mold.MoldDeliverProgressType;
import com.shop.tbms.enumerate.step.ReportType;
import com.shop.tbms.enumerate.step.StepStatus;
import com.shop.tbms.repository.MoldDeliverProgressRepository;
import com.shop.tbms.repository.MoldGroupElementProgressRepository;
import com.shop.tbms.repository.MoldProgressRepository;
import com.shop.tbms.repository.StepRepository;
import com.shop.tbms.util.ProgressUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Component
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

    public List<MoldProgressDTO> setCanCheckForMoldProgress(Step preStep, List<MoldProgressDTO> moldProgressDTOList) {
        return moldProgressDTOList.stream().map(moldProgressDTO -> {
            boolean canCheck = canCheckCompleteBySize(preStep, moldProgressDTO.getMoldSize());
            log.info("Set value canCheck of {} is {}", moldProgressDTO, canCheck);
            moldProgressDTO.setCanCheck(canCheck);

            return moldProgressDTO;
        }).collect(Collectors.toList());
    }

    public List<MoldElementProgressDTO> setCanCheckForMoldElementProgress(Step preStep, List<MoldElementProgressDTO> moldElementProgressDTOList) {
        return moldElementProgressDTOList.stream().map(moldElementProgressDTO -> {
            boolean canCheck = canCheckCompleteBySize(preStep, moldElementProgressDTO.getMoldSize());
            log.info("Set value canCheck of {} is {}", moldElementProgressDTO, canCheck);
            List<MoldElementProgressDetailDTO> moldElementProgressDetailDTOList = moldElementProgressDTO.getListElement().stream().map(moldElementProgressDetailDTO -> {
                moldElementProgressDetailDTO.setCanCheck(canCheck);
                return moldElementProgressDetailDTO;
            }).collect(Collectors.toList());

            moldElementProgressDTO.setListElement(moldElementProgressDetailDTOList);

            return moldElementProgressDTO;
        }).collect(Collectors.toList());
    }

    public List<MoldDeliverProgressDTO> setCanCheckForDeliveryProgress(Step preStep, List<MoldDeliverProgressDTO> moldDeliverProgressDTOList) {
        return moldDeliverProgressDTOList.stream().map(moldDeliverProgressDTO -> {
            boolean canCheck = canCheckCompleteBySize(preStep, moldDeliverProgressDTO.getMoldSize());
            log.info("Set value canCheck of {} is {}", moldDeliverProgressDTO, canCheck);
            moldDeliverProgressDTO.setCanCheck(canCheck);

            return moldDeliverProgressDTO;
        }).collect(Collectors.toList());
    }

    public boolean canCheckCompleteBySize(Step preStep, String moldSize) {
        switch (preStep.getReportType()) {
            case BY_MOLD:
                log.info("Check complete of mold size {} with preStep mold progress {}", moldSize, preStep.getListMoldProgress());
                return preStep.getListMoldProgress().stream()
                        .anyMatch(moldProgress ->
                                moldSize.equals(moldProgress.getMold().getSize())
                                    && Boolean.TRUE.equals(moldProgress.getIsCompleted())
                        );
            case BY_MOLD_ELEMENT:
                log.info("Check complete of mold size {} with preStep mold element progress {}", moldSize, preStep.getListMoldGroupElementProgresses());
                return preStep.getListMoldGroupElementProgresses().stream()
                        .filter(moldGroupElementProgress ->
                                moldSize.equals(moldGroupElementProgress.getMold().getSize()))
                        .allMatch(MoldGroupElementProgress::getIsCompleted);
            case BY_MOLD_SEND_RECEIVE:
                log.info("Check complete of mold size {} with preStep mold deliver progress {}", moldSize, preStep.getListMoldDeliverProgress());
                return preStep.getListMoldDeliverProgress().stream()
                        .filter(moldDeliverProgress ->
                                moldSize.equals(moldDeliverProgress.getMold().getSize()))
                        .allMatch(MoldDeliverProgress::getIsCompleted);
            default:
                return false;
        }
    }

    public void resetProgress(Step step, List<Mold> listMold) {
        log.info("Begin reset progress mold {} of step {}", listMold, step);

        /* set status to IN PROGRESS */
        if (!StepStatus.IN_PROGRESS.equals(step.getStatus())) step.setStatus(StepStatus.IN_PROGRESS);

        switch (step.getReportType()) {
            case BY_MOLD:
                resetMoldProgress(step, listMold);
                break;
            case BY_MOLD_SEND_RECEIVE:
                resetMoldGroupProgress(step, listMold);
                break;
            case BY_MOLD_ELEMENT:
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
            moldGroupElementProgressRepository.deleteByStepIdAndMoldId(step.getId(), mold.getId());

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
            Optional<MoldDeliverProgress> optinalProgress = progressList.stream()
                    .filter(moldProgress -> moldProgress.getMold().getId().equals(mold.getId()))
                    .findFirst();

            if (optinalProgress.isPresent()) {
                MoldDeliverProgress progress = optinalProgress.get();
                log.info("deliver progress is exited {}. Change complete to FALSE", progress);
                progress.setIsCompleted(Boolean.FALSE);

                listUpdatedProgress.add(progress);
            } else {
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
        listStepNeedToReset.parallelStream().forEach(step -> this.resetProgress(step, List.of(mold)));
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
        listStepNeedToReset.parallelStream().forEach(step -> this.resetMoldGroupProgressChangeElement(mold, step));
    }

    public void resetMoldGroupProgressChangeElement(Mold mold, Step step) {
        if (ReportType.BY_MOLD_ELEMENT.equals(step.getReportType())) {
            List<MoldGroupElement> elementList = mold.getMoldGroup().getListMoldGroupElement();
            List<MoldGroupElementProgress> listProgress = moldGroupElementProgressRepository.findAllByStepId(step.getId());

            List<MoldGroupElementProgress> deletedProgress = List.of();
            List<MoldGroupElementProgress> addedProgress = List.of();

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
}
