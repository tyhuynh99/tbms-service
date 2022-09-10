package com.shop.tbms.service.impl;

import com.shop.tbms.component.PurchaseOrderComponent;
import com.shop.tbms.component.StepComponent;
import com.shop.tbms.constant.MessageConstant;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.step.detail.StepDTO;
import com.shop.tbms.dto.step.report.ReportStepReqDTO;
import com.shop.tbms.dto.step.report_error.ReportErrorToStepRespDTO;
import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.OrderPaymentStatus;
import com.shop.tbms.enumerate.OrderStatus;
import com.shop.tbms.mapper.StepMapper;
import com.shop.tbms.mapper.StepSequenceMapper;
import com.shop.tbms.repository.*;
import com.shop.tbms.service.StepService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import javax.persistence.EntityNotFoundException;
import java.util.Comparator;
import java.util.List;

@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
public class StepServiceImpl implements StepService {
    @Autowired
    private StepMapper stepMapper;
    @Autowired
    private StepSequenceMapper stepSequenceMapper;

    @Autowired
    private StepRepository stepRepository;
    @Autowired
    private MoldProgressRepository moldProgressRepository;
    @Autowired
    private ChecklistRepository checklistRepository;
    @Autowired
    private MoldElementRepository moldElementRepository;
    @Autowired
    private PurchaseOrderRepository purchaseOrderRepository;
    @Autowired
    private StepSequenceRepository stepSequenceRepository;

    @Autowired
    private StepComponent stepComponent;
    @Autowired
    private PurchaseOrderComponent purchaseOrderComponent;

    @Autowired
    private MessageConstant messageConstant;

    @Override
    public StepDTO getStep(Long stepId) {
        Step step = stepRepository.findById(stepId).orElseThrow(EntityNotFoundException::new);
        return stepMapper.toDTO(step);
    }

    @Override
    public SuccessRespDTO reportStepProgress(ReportStepReqDTO reportStepReqDTO) {
        Step currentStep = stepRepository.findById(reportStepReqDTO.getStepId()).orElseThrow(EntityNotFoundException::new);
        PurchaseOrder currentOrder = currentStep.getProcedure().getPurchaseOrder();
        List<MoldProgress> currentMoldProgress = moldProgressRepository.findAllByStepId(currentStep.getId());
        List<Checklist> currentChecklist = currentStep.getListChecklist();
        List<Evidence> currentEvidence = currentStep.getListEvidence();
        List<MoldElement> currentMoldElement = currentOrder.getListMoldElement();

        /* validate */
        purchaseOrderComponent.canUpdateOrder(currentOrder);
        stepComponent.canReportProgress(currentStep);

        if (Boolean.TRUE.equals(currentStep.getIsEnd())) {
            // TODO: validate complete all step
            currentOrder.setStatus(OrderStatus.COMPLETED);

            if (Boolean.TRUE.equals(reportStepReqDTO.getIsPaid())) {
                currentOrder.setPaymentStatus(OrderPaymentStatus.PAID);
            } else {
                currentOrder.setPaymentStatus(OrderPaymentStatus.NOT_PAID);
            }

            purchaseOrderRepository.save(currentOrder);
        }

        stepComponent.updateMoldProgress(currentMoldProgress, reportStepReqDTO.getMoldProgress());
        moldProgressRepository.saveAll(currentMoldProgress);

        stepComponent.updateChecklist(currentChecklist, reportStepReqDTO.getChecklist());
        checklistRepository.saveAll(currentChecklist);

        stepComponent.updateMoldElement(currentStep, currentMoldElement, reportStepReqDTO.getMoleElement());
        moldElementRepository.saveAll(currentMoldElement);

        stepComponent.updateEvidence(currentStep, reportStepReqDTO.getEvidence());

        stepComponent.updateStep(currentStep, reportStepReqDTO);
        stepComponent.updateStepStatus(currentStep, currentMoldProgress);
        stepRepository.save(currentStep);

        // TODO: check generate next step progress

        return SuccessRespDTO.builder()
                .message(messageConstant.getUpdateSuccess())
                .build();
    }

    @Override
    public List<ReportErrorToStepRespDTO> getListToStepInReportError(Long stepId) {
        List<StepSequence> listStepSequence = stepSequenceRepository.findByStepBeforeId(stepId);

        /* return empty if there is only one option for step */
        if (CollectionUtils.isEmpty(listStepSequence) || listStepSequence.size() == 1) return List.of();

        listStepSequence.sort(Comparator.comparing(o -> o.getStepAfter().getSequenceNo()));
        listStepSequence.remove(listStepSequence.size() - 1);

        return stepSequenceMapper.toToNextStepErrors(listStepSequence);
    }
}
