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

    public void updateMoldProgress(Step currentStep, List<ReportProgressReqDTO> listReq) {
        List<MoldProgress> progressList = currentStep.getListMoldProgress();

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
                                Step nextStep = Objects.requireNonNull(StepUtil.getNextMainStep(currentStep.getListStepAfter()));
                                boolean canCheckComplete = progressComponent.canUnCheckCompleteBySize(
                                        nextStep,
                                        moldProgress.getMold().getSize()
                                );

                                if (!canCheckComplete) {
                                    log.error("Mold {} is complete in next step {}", moldProgress.getMold(), nextStep);
                                    throw new BusinessException("Mold " + moldProgress.getMold().getSize() +  " is complete in next step");
                                }
                            }

                            moldProgress.setIsCompleted(Boolean.FALSE);
                        } else if (Boolean.FALSE.equals(moldProgress.getIsCompleted()) && Boolean.TRUE.equals(reqDTO.getIsCompleted())) {
                            /* update to complete */
                            log.info("Update progress {} to complete", moldProgress);

                            /* validate complete in pre-step */
                            if (Boolean.FALSE.equals(currentStep.getIsStart())) {
                                Step preStep = Objects.requireNonNull(StepUtil.getPreMainStep(currentStep.getListStepBefore()));
                                boolean canCheckComplete = progressComponent.canCheckCompleteBySize(
                                        preStep,
                                        moldProgress.getMold().getSize()
                                );

                                if (!canCheckComplete) {
                                    log.error("Mold {} is not complete in prestep {}", moldProgress.getMold(), preStep);
                                    throw new BusinessException("Mold " + moldProgress.getMold().getSize() +  " is not complete in prestep");
                                }
                            }

                            moldProgress.setIsCompleted(Boolean.TRUE);
                        }
                    }
                });
    }

    public void updateMoldElementProgress(Step currentStep, List<ReportProgressReqDTO> listReq) {
        List<MoldGroupElementProgress> listNotComplete = currentStep.getListMoldGroupElementProgresses().stream()
                .filter(Predicate.not(MoldGroupElementProgress::getIsCompleted))
                .collect(Collectors.toList());

        listNotComplete.forEach(moldGroupElementProgress -> {
            boolean isUpdateToComplete = listReq.stream().anyMatch(reportProgressReqDTO ->
                    moldGroupElementProgress.getId().equals(reportProgressReqDTO.getProgressId())
                            && Boolean.TRUE.equals(reportProgressReqDTO.getIsCompleted()));

            if (isUpdateToComplete) {
                /* validate complete in prestep */
                if (Boolean.FALSE.equals(currentStep.getIsStart())) {
                    Step preStep = Objects.requireNonNull(StepUtil.getPreMainStep(currentStep.getListStepBefore()));
                    boolean canCheckComplete = progressComponent.canCheckCompleteBySize(
                            preStep,
                            moldGroupElementProgress.getMold().getSize()
                    );

                    if (!canCheckComplete) {
                        log.error("Mold {} is not complete in prestep {}", moldGroupElementProgress.getMold(), preStep);
                        throw new BusinessException("Mold " + moldGroupElementProgress.getMold().getSize() +  " is not complete in prestep");
                    }
                }

                moldGroupElementProgress.setIsCompleted(Boolean.TRUE);
            }
        });
    }

    public void updateMoldDeliverProgress(Step currentStep, List<ReportProgressReqDTO> listReq) {
        List<MoldDeliverProgress> listNotComplete = currentStep.getListMoldDeliverProgress().stream()
                .filter(Predicate.not(MoldDeliverProgress::getIsCompleted)).
                collect(Collectors.toList());

        listNotComplete.forEach(moldDeliverProgress -> {
            boolean isUpdateToComplete = listReq.stream().anyMatch(reportProgressReqDTO ->
                    moldDeliverProgress.getId().equals(reportProgressReqDTO.getProgressId())
                            && Boolean.TRUE.equals(reportProgressReqDTO.getIsCompleted()));

            if (isUpdateToComplete) {
                /* validate complete in prestep */
                if (Boolean.FALSE.equals(currentStep.getIsStart())) {
                    Step preStep = Objects.requireNonNull(StepUtil.getPreMainStep(currentStep.getListStepBefore()));
                    boolean canCheckComplete = progressComponent.canCheckCompleteBySize(
                            preStep,
                            moldDeliverProgress.getMold().getSize()
                    );

                    if (!canCheckComplete) {
                        log.error("Mold {} is not complete in prestep {}", moldDeliverProgress.getMold(), preStep);
                        throw new BusinessException("Mold " + moldDeliverProgress.getMold().getSize() +  " is not complete in prestep");
                    }
                }

                moldDeliverProgress.setIsCompleted(Boolean.TRUE);
            }
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
}
