package com.shop.tbms.component;

import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.constant.LogConstant;
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
import com.shop.tbms.util.ReportLogUtil;
import com.shop.tbms.util.StepUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.time.LocalDate;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static com.shop.tbms.constant.AppConstant.ZERO;

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
    @Autowired
    private LogConstant logConstant;

    @Autowired
    private ProgressComponent progressComponent;

    public List<OrderStepRespDTO> setPercentProgress(List<OrderStepRespDTO> listStepDTO, List<Step> listOriginStep) {
        for (OrderStepRespDTO respDTO : listStepDTO) {
            Step originStep = StepUtil.findStepById(respDTO.getId(), listOriginStep);

            if (StepStatus.INIT.equals(originStep.getStatus())) {
                respDTO.setPercentComplete(ZERO);
            } else {
                respDTO.setPercentComplete(StepUtil.calPercentComplete(originStep));
            }
        }
        return listStepDTO;
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
            currentStep.setFactoryName(req.getFactoryName());
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

    public void updateMoldProgress(Step currentStep, List<ReportProgressReqDTO> listReq, List<String> logDetail) {
        List<MoldProgress> progressList = currentStep.getListMoldProgress();
        final List<MoldProgress> listUpdateToComplete = new ArrayList<>();
        final List<MoldProgress> listUpdateToUnComplete = new ArrayList<>();

        progressList.stream()
                .forEach(moldProgress -> {
                    Optional<ReportProgressReqDTO> reqChk = listReq.stream()
                            .filter(reportProgressReqDTO ->
                                    moldProgress.getId().equals(reportProgressReqDTO.getProgressId()))
                            .findFirst();

                    if (reqChk.isPresent()) {
                        ReportProgressReqDTO reqDTO = reqChk.get();

                        if (Boolean.TRUE.equals(moldProgress.getIsCompleted()) && Boolean.FALSE.equals(reqDTO.getIsCompleted())) {
                            /* update to incomplete */
                            log.info("Update progress {} to incomplete", moldProgress);

                            /* validate not complete in next-step */
                            if (Boolean.FALSE.equals(currentStep.getIsEnd())) {
                                Step nextStep = Objects.requireNonNull(StepUtil.getNextMainStep(currentStep.getListStepBefore(), moldProgress.getMold()));
                                boolean canCheckComplete = progressComponent.canUnCheckCompleteBySize(
                                        nextStep,
                                        moldProgress.getMold().getSize()
                                );

                                if (!canCheckComplete) {
                                    log.error("Mold {} is complete in next step {}", moldProgress.getMold(), nextStep);
                                    throw new BusinessException("Mold " + moldProgress.getMold().getSize() + " is complete in next step");
                                }
                            }

                            listUpdateToUnComplete.add(moldProgress);
                            moldProgress.setIsCompleted(Boolean.FALSE);
                        } else if (Boolean.FALSE.equals(moldProgress.getIsCompleted()) && Boolean.TRUE.equals(reqDTO.getIsCompleted())) {
                            /* update to complete */
                            log.info("Update progress {} to complete", moldProgress);

                            /* validate complete in pre-step */
                            if (Boolean.FALSE.equals(currentStep.getIsStart())) {
                                Step preStep = Objects.requireNonNull(StepUtil.getPreMainStep(currentStep.getListStepAfter(), moldProgress.getMold()));
                                boolean canCheckComplete = progressComponent.canCheckCompleteBySize(
                                        preStep,
                                        moldProgress.getMold().getSize()
                                );

                                if (!canCheckComplete) {
                                    log.error("Mold {} is not complete in prestep {}", moldProgress.getMold(), preStep);
                                    throw new BusinessException("Mold " + moldProgress.getMold().getSize() + " is not complete in prestep");
                                }
                            }

                            listUpdateToComplete.add(moldProgress);
                            moldProgress.setIsCompleted(Boolean.TRUE);
                        }
                    }
                });

        logDetail.addAll(ReportLogUtil.genLogForMoldProgress(listUpdateToComplete, listUpdateToUnComplete, logConstant));
    }

    public void updateMoldElementProgress(Step currentStep, List<ReportProgressReqDTO> listReq, List<String> logDetail) {
        List<MoldGroupElementProgress> progressList = currentStep.getListMoldGroupElementProgresses();
        final List<MoldGroupElementProgress> listUpdateToComplete = new ArrayList<>();
        final List<MoldGroupElementProgress> listUpdateToUnComplete = new ArrayList<>();

        progressList.forEach(moldGroupElementProgress -> {
            Optional<ReportProgressReqDTO> reqChk = listReq.stream()
                    .filter(reportProgressReqDTO ->
                            moldGroupElementProgress.getId().equals(reportProgressReqDTO.getProgressId()))
                    .findFirst();

            if (reqChk.isPresent()) {
                ReportProgressReqDTO reqDTO = reqChk.get();

                if (Boolean.TRUE.equals(moldGroupElementProgress.getIsCompleted()) && Boolean.FALSE.equals(reqDTO.getIsCompleted())) {
                    /* update to incomplete */
                    log.info("Update progress {} to incomplete", moldGroupElementProgress);

                    /* validate not complete in next-step */
                    if (Boolean.FALSE.equals(currentStep.getIsEnd())) {
                        Step nextStep = Objects.requireNonNull(StepUtil.getNextMainStep(currentStep.getListStepAfter(), moldGroupElementProgress.getMold()));
                        boolean canCheckComplete = progressComponent.canUnCheckCompleteBySize(
                                nextStep,
                                moldGroupElementProgress.getMold().getSize()
                        );

                        if (!canCheckComplete) {
                            log.error("Mold {} is complete in next step {}", moldGroupElementProgress.getMold(), nextStep);
                            throw new BusinessException("Mold " + moldGroupElementProgress.getMold().getSize() + " is complete in next step");
                        }
                    }

                    listUpdateToUnComplete.add(moldGroupElementProgress);
                    moldGroupElementProgress.setIsCompleted(Boolean.FALSE);
                } else if (Boolean.FALSE.equals(moldGroupElementProgress.getIsCompleted()) && Boolean.TRUE.equals(reqDTO.getIsCompleted())) {
                    /* update to complete */
                    log.info("Update progress {} to complete", moldGroupElementProgress);

                    /* validate complete in pre-step */
                    if (Boolean.FALSE.equals(currentStep.getIsStart())) {
                        Step preStep = Objects.requireNonNull(StepUtil.getPreMainStep(currentStep.getListStepBefore(), moldGroupElementProgress.getMold()));
                        boolean canCheckComplete = progressComponent.canCheckCompleteBySize(
                                preStep,
                                moldGroupElementProgress.getMold().getSize()
                        );

                        if (!canCheckComplete) {
                            log.error("Mold {} is not complete in prestep {}", moldGroupElementProgress.getMold(), preStep);
                            throw new BusinessException("Mold " + moldGroupElementProgress.getMold().getSize() + " is not complete in prestep");
                        }
                    }

                    listUpdateToComplete.add(moldGroupElementProgress);
                    moldGroupElementProgress.setIsCompleted(Boolean.TRUE);
                }
            }
        });

        logDetail.addAll(ReportLogUtil.genLogForMoldElementProgress(listUpdateToComplete, listUpdateToUnComplete, logConstant));
    }

    public void updateMoldDeliverProgress(Step currentStep, List<ReportProgressReqDTO> listReq, List<String> logDetail) {
        List<MoldDeliverProgress> progressList = currentStep.getListMoldDeliverProgress().stream()
                .filter(Predicate.not(MoldDeliverProgress::getIsCompleted)).
                collect(Collectors.toList());
        final List<MoldDeliverProgress> listUpdateToComplete = new ArrayList<>();
        final List<MoldDeliverProgress> listUpdateToUnComplete = new ArrayList<>();

        progressList.forEach(moldDeliverProgress -> {
            Optional<ReportProgressReqDTO> reqChk = listReq.stream()
                    .filter(reportProgressReqDTO ->
                            moldDeliverProgress.getId().equals(reportProgressReqDTO.getProgressId()))
                    .findFirst();

            if (reqChk.isPresent()) {
                ReportProgressReqDTO reqDTO = reqChk.get();

                if (Boolean.TRUE.equals(moldDeliverProgress.getIsCompleted()) && Boolean.FALSE.equals(reqDTO.getIsCompleted())) {
                    /* update to incomplete */
                    log.info("Update progress {} to incomplete", moldDeliverProgress);

                    /* validate not complete in next-step */
                    if (Boolean.FALSE.equals(currentStep.getIsEnd())) {
                        Step nextStep = Objects.requireNonNull(StepUtil.getNextMainStep(currentStep.getListStepAfter(), moldDeliverProgress.getMold()));
                        boolean canCheckComplete = progressComponent.canUnCheckCompleteBySize(
                                nextStep,
                                moldDeliverProgress.getMold().getSize()
                        );

                        if (!canCheckComplete) {
                            log.error("Mold {} is complete in next step {}", moldDeliverProgress.getMold(), nextStep);
                            throw new BusinessException("Mold " + moldDeliverProgress.getMold().getSize() + " is complete in next step");
                        }
                    }

                    listUpdateToUnComplete.add(moldDeliverProgress);
                    moldDeliverProgress.setIsCompleted(Boolean.FALSE);
                } else if (Boolean.FALSE.equals(moldDeliverProgress.getIsCompleted()) && Boolean.TRUE.equals(reqDTO.getIsCompleted())) {
                    /* update to complete */
                    log.info("Update progress {} to complete", moldDeliverProgress);

                    /* validate complete in pre-step */
                    if (Boolean.FALSE.equals(currentStep.getIsStart())) {
                        Step preStep = Objects.requireNonNull(StepUtil.getPreMainStep(currentStep.getListStepBefore(), moldDeliverProgress.getMold()));
                        boolean canCheckComplete = progressComponent.canCheckCompleteBySize(
                                preStep,
                                moldDeliverProgress.getMold().getSize()
                        );

                        if (!canCheckComplete) {
                            log.error("Mold {} is not complete in prestep {}", moldDeliverProgress.getMold(), preStep);
                            throw new BusinessException("Mold " + moldDeliverProgress.getMold().getSize() + " is not complete in prestep");
                        }
                    }

                    listUpdateToComplete.add(moldDeliverProgress);
                    moldDeliverProgress.setIsCompleted(Boolean.TRUE);
                }
            }
        });
        logDetail.addAll(ReportLogUtil.genLogForMoldDeliverProgress(listUpdateToComplete, listUpdateToUnComplete, logConstant));
    }

    public void updateChecklist(List<Checklist> currentChecklist, List<ReportChecklistReqDTO> listReq, List<String> logDetail) {
        final List<Checklist> listChangeToChecked = new ArrayList<>();
        final List<Checklist> listChangeToUnchecked = new ArrayList<>();

        currentChecklist.forEach(checklist -> {
            Optional<ReportChecklistReqDTO> chkReq = listReq.stream()
                    .filter(req -> checklist.getId().equals(req.getChecklistId()))
                    .findFirst();

            if (chkReq.isPresent()) {
                ReportChecklistReqDTO reqDTO = chkReq.get();

                if (!Objects.equals(checklist.getIsChecked(), reqDTO.getIsChecked())) {
                    log.info("Update checklist req {} to {}", reqDTO, checklist);

                    if (Boolean.TRUE.equals(reqDTO.getIsChecked())) {
                        listChangeToChecked.add(checklist);
                    } else {
                        listChangeToUnchecked.add(checklist);
                    }
                    checklist.setIsChecked(reqDTO.getIsChecked());
                }
            }
        });

        logDetail.addAll(ReportLogUtil.genLogForCheckList(listChangeToChecked, listChangeToUnchecked, logConstant));
    }

    public void updateEvidence(Step currentStep, ReportEvidenceReqDTO reqDTO, ReportLog reportLog) {
        List<Evidence> currentEvidence = currentStep.getListEvidence();
        log.info("Update Evidence with current {} and req {}", currentEvidence, reqDTO);
        if (Objects.isNull(reqDTO.getListDeleteEvidenceId())) reqDTO.setListDeleteEvidenceId(new ArrayList<>());

        /* delete evidence */
        List<Evidence> listDeleteEvidence = currentEvidence.stream()
                .filter(evidence ->
                        reqDTO.getListDeleteEvidenceId()
                                .contains(evidence.getId()))
                .map(evidence -> {
                    evidence.setIsDelete(Boolean.TRUE);
                    evidence.setDeleteAtReportLog(reportLog);
                    return evidence;
                }).collect(Collectors.toList());
        evidenceRepository.saveAll(listDeleteEvidence);

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
                        Boolean.FALSE,
                        currentStep,
                        reportLog,
                        null
                );
            }).collect(Collectors.toList());

            /* save to has evidence id */
            evidenceRepository.saveAll(listNewEvidence);

            listNewEvidence.forEach(evidence -> evidence.setFilename(EvidenceUtil.generateFilename(evidence)));

            /* update file name */
            evidenceRepository.saveAll(listNewEvidence);
        }
    }
}
