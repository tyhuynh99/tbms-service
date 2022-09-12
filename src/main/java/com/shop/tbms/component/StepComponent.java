package com.shop.tbms.component;

import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.constant.MathConstant;
import com.shop.tbms.constant.StepConstant;
import com.shop.tbms.dto.order.OrderStepRespDTO;
import com.shop.tbms.dto.step.report.*;
import com.shop.tbms.dto.step.report_issue.ReportIssueStepReqDTO;
import com.shop.tbms.dto.step.upd_expect_date.UpdateExpectedCompleteReqDTO;
import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.OrderStatus;
import com.shop.tbms.enumerate.StepStatus;
import com.shop.tbms.enumerate.StepType;
import com.shop.tbms.repository.MoldProgressRepository;
import com.shop.tbms.repository.PurchaseOrderRepository;
import com.shop.tbms.repository.TemplateMoldElementRepository;
import com.shop.tbms.util.MoldElementUtil;
import com.shop.tbms.util.StepUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

@Component
@Slf4j
public class StepComponent {
    @Autowired
    private MoldProgressRepository moldProgressRepository;
    @Autowired
    private PurchaseOrderRepository purchaseOrderRepository;
    @Autowired
    private TemplateMoldElementRepository templateMoldElementRepository;

    @Autowired
    private StepConstant stepConstant;

    public List<OrderStepRespDTO> setPercentProgress(List<OrderStepRespDTO> listStepDTO, List<Step> listOriginStep) {
        for (OrderStepRespDTO respDTO : listStepDTO) {
            Step originStep = StepUtil.findStepById(respDTO.getId(), listOriginStep);

            if (StepStatus.INIT.equals(originStep.getStatus())) {
                respDTO.setPercentComplete(BigDecimal.ZERO);
            } else {
                respDTO.setPercentComplete(calPercent(originStep));
            }
        }
        return listStepDTO;
    }

    private BigDecimal calPercent(Step step) {
        List<MoldProgress> listMoldProgress = moldProgressRepository.findAllByStepId(step.getId());

        if (CollectionUtils.isEmpty(listMoldProgress)) return BigDecimal.ZERO;

        BigDecimal completedMold = BigDecimal.valueOf(listMoldProgress.stream()
                .filter(moldProgress -> Boolean.TRUE.equals(moldProgress.getIsCompleted()))
                .count());
        BigDecimal totalMold = BigDecimal.valueOf(listMoldProgress.size());

        return completedMold.divide(totalMold, MathConstant.SCALE, MathConstant.ROUNDING_MODE);
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

    public void updateMoldElement(Step currentStep, List<MoldElement> currentMoldElement, List<ReportMoldElementReqDTO> listReq) {
        /* validate require update mold element */
        if (stepConstant.getListStepNeedUpdateMoldElement().containsKey(currentStep.getCode())) {
            if (CollectionUtils.isEmpty(listReq)) {
                throw new BusinessException(
                        String.format(
                                "Step %s required mold element.",
                                stepConstant.getListStepNeedUpdateMoldElement().get(currentStep.getCode())
                        )
                );
            }
        } else {
            if (!CollectionUtils.isEmpty(listReq)) {
                throw new BusinessException(String.format("Step %s is not allowed to update mold element", currentStep.getName()));
            }
        }

        /* create map for get mold element by code */
        Map<String, MoldElement> mappedElement = new HashMap<>();
        currentMoldElement.forEach(element -> mappedElement.put(element.getCode(), element));

        listReq.forEach(reqDTO -> {
            MoldElement curElement = mappedElement.get(reqDTO.getCode());
            if (Objects.isNull(curElement)) {
                throw new BusinessException("Not found element with code " + reqDTO.getCode());
            }

            /* validate input */
            MoldElementUtil.validateElementDescription(curElement, reqDTO);

            /* update data */
            curElement.setDescription(reqDTO.getDescription());
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

    public void updateEvidence(Step currentStep, List<ReportEvidenceReqDTO> listReq) {
//        List<Evidence> currentEvidence = currentStep.getListEvidence();
//
//        /* validate required evidence */
//        if (Boolean.TRUE.equals(currentStep.getRequiredEvidence())) {
//            if (CollectionUtils.isEmpty(listReq)) {
//                throw new BusinessException("Step require evidences");
//            }
//        }
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
