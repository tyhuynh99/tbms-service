package com.shop.tbms.component;

import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.constant.StepConstant;
import com.shop.tbms.dto.FileDTO;
import com.shop.tbms.dto.order.OrderStepRespDTO;
import com.shop.tbms.dto.step.report.*;
import com.shop.tbms.dto.step.upd_expect_date.UpdateExpectedCompleteReqDTO;
import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.order.OrderStatus;
import com.shop.tbms.enumerate.step.StepStatus;
import com.shop.tbms.enumerate.step.StepType;
import com.shop.tbms.repository.EvidenceRepository;
import com.shop.tbms.repository.MoldProgressRepository;
import com.shop.tbms.repository.PurchaseOrderRepository;
import com.shop.tbms.service.FileService;
import com.shop.tbms.util.EvidenceUtil;
import com.shop.tbms.util.StepUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

import static com.shop.tbms.constant.AppConstant.ZERO_LONG;

@Component
@Slf4j
public class StepComponent {
    @Autowired
    private FileService fileService;

    @Autowired
    private MoldProgressRepository moldProgressRepository;
    @Autowired
    private PurchaseOrderRepository purchaseOrderRepository;
    @Autowired
    private EvidenceRepository evidenceRepository;

    @Autowired
    private StepConstant stepConstant;

    public List<OrderStepRespDTO> setPercentProgress(List<OrderStepRespDTO> listStepDTO, List<Step> listOriginStep) {
        for (OrderStepRespDTO respDTO : listStepDTO) {
            Step originStep = StepUtil.findStepById(respDTO.getId(), listOriginStep);

            if (StepStatus.INIT.equals(originStep.getStatus())) {
                respDTO.setPercentComplete(ZERO_LONG);
            } else {
                respDTO.setPercentComplete(calPercent(originStep));
            }
        }
        return listStepDTO;
    }

    private Long calPercent(Step step) {
        List<MoldProgress> listMoldProgress = moldProgressRepository.findAllByStepId(step.getId());

        if (CollectionUtils.isEmpty(listMoldProgress)) return ZERO_LONG;

        long completedMold = listMoldProgress.stream()
                .filter(moldProgress -> Boolean.TRUE.equals(moldProgress.getIsCompleted()))
                .count();
        long totalMold =listMoldProgress.size();

        return Math.floorDiv(completedMold, totalMold);
    }

    public void validateUpdateExpectedCompleteData(List<UpdateExpectedCompleteReqDTO> listReqDTO, List<Step> listCurStep) {
        /* Only update step of order status = IN PROGRESS */
        List<PurchaseOrder> listOrder = listCurStep.stream()
                .map(Step::getProcedure)
                .map(Procedure::getPurchaseOrder)
                .distinct()
                .collect(Collectors.toList());

        boolean isInvalidOrderStatus = listOrder.stream()
                .anyMatch(purchaseOrder ->
                        !OrderStatus.IN_PROGRESS.equals(purchaseOrder.getStatus())
                                ||
                                Boolean.TRUE.equals(purchaseOrder.getIsDeleted())
                );

        if (isInvalidOrderStatus) {
            throw new BusinessException("Invalid request. Contains completed order.");
        }

        /* if any expected date is in the past -> INVALID */
        boolean isReqDateInvalid = listReqDTO.stream()
                .map(UpdateExpectedCompleteReqDTO::getExpectedCompleteDate)
                .anyMatch(reqDate -> LocalDate.now().isAfter(reqDate));

        if (isReqDateInvalid) {
            throw new BusinessException("Invalid request. Contains date in the past.");
        }
    }

    public void canReportProgress(Step currentStep) {
        /* check step status */
        if (StepStatus.COMPLETED.equals(currentStep.getStatus())) {
            throw new BusinessException(
                    String.format(
                            "Current step {} status is {}. Only status not Completed can be reported.",
                            currentStep.getCode(),
                            currentStep.getStatus()
                    )
            );
        }
    }

    public void updateStep(Step currentStep, ReportStepReqDTO req) {
        currentStep.setNote(req.getNote());

        if (StepType.THIRD_PARTY.equals(currentStep.getType())) {
            currentStep.setDeliveredDate(req.getDeliveredDate());
            currentStep.setReceivedDate(req.getReceivedDate());
        }

        if (Boolean.TRUE.equals(currentStep.getIsEnd())) {
            currentStep.setExportDate(req.getExportDate());
            currentStep.setExpectedPaidDate(req.getExpectedPaidDate());
        }
    }

    public void updateStepStatus(Step currentStep, List<MoldProgress> listMoldProgress) {
        boolean isAllMoldComplete = listMoldProgress.stream()
                .allMatch(progress -> Boolean.TRUE.equals(progress.getIsCompleted()));

        if (isAllMoldComplete) {
            currentStep.setStatus(StepStatus.COMPLETED);
        }
    }

    public void updateMoldProgress(List<MoldProgress> currentMoldProgress, List<ReportMoldProgressReqDTO> listReq) {
        currentMoldProgress.forEach(currentProgress -> {
            currentProgress.setIsCompleted(
                    listReq.stream()
                            .filter(req -> currentProgress.getId().equals(req.getProgressId()))
                            .map(ReportMoldProgressReqDTO::getIsCompleted)
                            .findFirst()
                            .orElse(currentProgress.getIsCompleted())
            );
        });
    }

    public void updateChecklist(List<Checklist> currentChecklist, List<ReportChecklistReqDTO> listReq) {
        currentChecklist.forEach(checklist -> {
            checklist.setIsChecked(
                    listReq.stream()
                            .filter(req -> checklist.getId().equals(req.getChecklistId()))
                            .map(ReportChecklistReqDTO::getIsChecked)
                            .findFirst()
                            .orElse(checklist.getIsChecked())
            );
        });
    }

    public void updateEvidence(Step currentStep, ReportEvidenceReqDTO reqDTO) {
        List<Evidence> currentEvidence = currentStep.getListEvidence();
        log.info("Update Evidence with current {} and req {}", currentEvidence, reqDTO);
        if (Objects.isNull(reqDTO.getListDeleteEvidenceId())) reqDTO.setListDeleteEvidenceId(new ArrayList<>());

        /* validate delete evidence */
        List<Evidence> listDeleteEvidence = currentEvidence.stream()
                .filter(evidence ->
                        reqDTO.getListDeleteEvidenceId()
                                .contains(evidence.getId()))
                .collect(Collectors.toList());
        /* delete evidence */
        if (!CollectionUtils.isEmpty(listDeleteEvidence)) {
            evidenceRepository.deleteAll(listDeleteEvidence);
        }

        /* validate required evidence */
        if (Boolean.TRUE.equals(currentStep.getRequiredEvidence())) {
            if (CollectionUtils.isEmpty(reqDTO.getListFile()) && currentEvidence.size() == listDeleteEvidence.size()) {
                throw new BusinessException("Step require evidences");
            }
        }

        if (!CollectionUtils.isEmpty(reqDTO.getListFile())) {
            /* upload file */
            List<Evidence> listNewEvidence = reqDTO.getListFile().parallelStream().map(multipartFile -> {
                FileDTO fileDTO = null;
                try {
                    fileDTO = fileService.upload(multipartFile);
                } catch (Exception e) {
                    log.error("Upload file {} has exception {}", multipartFile.getOriginalFilename(), e);
                    throw new RuntimeException(e);
                }
                return new Evidence(
                        null,
                        null,
                        multipartFile.getOriginalFilename(),
                        fileDTO.getUrl(),
                        currentStep
                );
            }).collect(Collectors.toList());

            /* save to has evidence id */
            evidenceRepository.saveAll(listNewEvidence);

            listNewEvidence.forEach(evidence -> evidence.setFilename(EvidenceUtil.generateFilename(evidence)));

            /* update file name */
            evidenceRepository.saveAll(listNewEvidence);
        }
    }

    public void resetProgress(Step step, List<Mold> listMold) {
        log.info("Begin reset progress mold {} of step {}", listMold, step);

        /* set status to IN PROGRESS */
        if (!StepStatus.IN_PROGRESS.equals(step.getStatus())) step.setStatus(StepStatus.IN_PROGRESS);

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
        log.info("End reset mold progress to fixing step.");
    }
}
