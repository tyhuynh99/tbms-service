package com.shop.tbms.service.impl;

import com.shop.tbms.component.*;
import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.constant.MessageConstant;
import com.shop.tbms.dto.MoldDTO;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.step.ResetMoldStepReqDTO;
import com.shop.tbms.dto.step.detail.StepDTO;
import com.shop.tbms.dto.step.report.ReportStepReqDTO;
import com.shop.tbms.dto.step.report_issue.ReportIssueStepReqDTO;
import com.shop.tbms.dto.step.report_issue.ReportIssueToStepRespDTO;
import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.OrderPaymentStatus;
import com.shop.tbms.enumerate.OrderStatus;
import com.shop.tbms.enumerate.StepStatus;
import com.shop.tbms.mapper.MoldMapper;
import com.shop.tbms.mapper.StepMapper;
import com.shop.tbms.mapper.StepSequenceMapper;
import com.shop.tbms.repository.*;
import com.shop.tbms.service.StepService;
import com.shop.tbms.util.StepUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import javax.persistence.EntityNotFoundException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
@Slf4j
public class StepServiceImpl implements StepService {
    @Autowired
    private StepMapper stepMapper;
    @Autowired
    private StepSequenceMapper stepSequenceMapper;
    @Autowired
    private MoldMapper moldMapper;

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
    private IssueRepository issueRepository;
    @Autowired
    private MoldRepository moldRepository;

    @Autowired
    private StepComponent stepComponent;
    @Autowired
    private PurchaseOrderComponent purchaseOrderComponent;
    @Autowired
    private ReportLogComponent reportLogComponent;
    @Autowired
    private IssueComponent issueComponent;
    @Autowired
    private MoldComponent moldComponent;

    @Autowired
    private MessageConstant messageConstant;

    @Override
    public StepDTO getStep(Long stepId) {
        Step step = stepRepository.findById(stepId).orElseThrow(EntityNotFoundException::new);
        return stepMapper.toDTO(step);
    }

    @Override
    public SuccessRespDTO reportStepProgress(ReportStepReqDTO reportStepReqDTO) {
        log.info("Begin report step progress {}", reportStepReqDTO);
        Step currentStep = stepRepository.findById(reportStepReqDTO.getStepId()).orElseThrow(EntityNotFoundException::new);
        PurchaseOrder currentOrder = currentStep.getProcedure().getPurchaseOrder();
        List<MoldProgress> currentMoldProgress = moldProgressRepository.findAllByStepId(currentStep.getId());
        List<Checklist> currentChecklist = currentStep.getListChecklist();
        List<Evidence> currentEvidence = currentStep.getListEvidence();
        List<MoldElement> currentMoldElement = currentOrder.getListMoldElement();

        /* validate */
        /* validate order status */
        purchaseOrderComponent.canUpdateOrder(currentOrder);

        /* validate step status */
        stepComponent.canReportProgress(currentStep);
        moldComponent.validateMoldProgress(
                getListCompletedMoldInPreviousStep(reportStepReqDTO.getStepId()),
                reportStepReqDTO.getMoldProgress());

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

        stepComponent.updateMoldElement(currentStep, currentMoldElement, reportStepReqDTO.getMoldElement());
        moldElementRepository.saveAll(currentMoldElement);

        stepComponent.updateEvidence(currentStep, reportStepReqDTO.getEvidence());

        stepComponent.updateStep(currentStep, reportStepReqDTO);
        stepComponent.updateStepStatus(currentStep, currentMoldProgress);
        stepRepository.save(currentStep);

        /* set status next step */
        Step nextMainStep = StepUtil.getNextMainStep(stepSequenceRepository.findByStepBeforeId(currentStep.getId()));
        if (!StepStatus.IN_PROGRESS.equals(nextMainStep.getStatus())) {
            nextMainStep.setStatus(StepStatus.IN_PROGRESS);
            stepRepository.save(nextMainStep);
        }

        /* insert log */
        reportLogComponent.insertReportLog(currentStep, reportStepReqDTO);

        log.info("End report step progress");

        return SuccessRespDTO.builder()
                .message(messageConstant.getUpdateSuccess())
                .build();
    }

    @Override
    public List<ReportIssueToStepRespDTO> getListToStepInReportError(Long stepId) {
        List<StepSequence> listStepSequence = stepSequenceRepository.findByStepBeforeId(stepId);

        /* return empty if there is only one option for step */
        if (CollectionUtils.isEmpty(listStepSequence) || listStepSequence.size() == 1) return List.of();

        listStepSequence.sort(Comparator.comparing(o -> o.getStepAfter().getSequenceNo()));
        listStepSequence.remove(listStepSequence.size() - 1);

        return stepSequenceMapper.toToNextStepErrors(listStepSequence);
    }

    @Override
    public SuccessRespDTO reportIssueStep(ReportIssueStepReqDTO reportIssueStepReqDTO) {
        log.info("Begin report error step {}", reportIssueStepReqDTO);
        boolean isNormalFlow = true;
        Step currentStep = stepRepository
                .findById(reportIssueStepReqDTO.getStepId())
                .orElseThrow(EntityNotFoundException::new);
        PurchaseOrder order = currentStep.getProcedure().getPurchaseOrder();
        List<Mold> listReportedMold;

        /* validate order status */
        purchaseOrderComponent.canUpdateOrder(order);

        /* validate step status */
        stepComponent.canReportProgress(currentStep);

        /* validate mold */
        if (CollectionUtils.isEmpty(reportIssueStepReqDTO.getListMoldId())) {
            throw new BusinessException("Required list mold");
        } else {
            listReportedMold = order.getListMold().stream()
                    .filter(mold -> reportIssueStepReqDTO.getListMoldId().contains(mold.getId()))
                    .collect(Collectors.toList());

            if (listReportedMold.size() != reportIssueStepReqDTO.getListMoldId().size()) {
                throw new BusinessException("Mold ID not existed in order");
            }
        }

        /* check type of next step. does next step has fixing step or normal step */
        /* normal step only need to store error report */
        /* fixing step need add mold to fixing step, or reset mold in previous step */
        List<ReportIssueToStepRespDTO> nextStepError = getListToStepInReportError(reportIssueStepReqDTO.getStepId());
        log.info("Check next step available has result {}", nextStepError);
        if (CollectionUtils.isEmpty(nextStepError)) {
            if (Objects.nonNull(reportIssueStepReqDTO.getChangeToStepId()) && reportIssueStepReqDTO.getChangeToStepId() != 0) {
                throw new BusinessException("Step cannot report error with next step id");
            }
        } else {
            boolean reqNextStepIsValid = nextStepError.stream()
                    .map(ReportIssueToStepRespDTO::getStepId)
                    .anyMatch(stepId -> stepId.equals(reportIssueStepReqDTO.getChangeToStepId()));

            if (!reqNextStepIsValid) {
                throw new BusinessException(
                        "Cannot found valid next step id with step id = "+ reportIssueStepReqDTO.getChangeToStepId()
                );
            }
            isNormalFlow = false;
        }

        /* save data report error */
        issueRepository.save(
                issueComponent.generateEntity(
                        reportIssueStepReqDTO,
                        order.getListMold(),
                        currentStep)
        );

        /* reset if not normal flow */
        if (!isNormalFlow) {
            Step resetStep = stepRepository.findById(reportIssueStepReqDTO.getChangeToStepId()).orElseThrow();
            stepComponent.resetProgress(resetStep, listReportedMold);
        }

        /* insert log */
        reportLogComponent.insertReportLog(currentStep, reportIssueStepReqDTO);

        return SuccessRespDTO.builder()
                .message(messageConstant.getUpdateSuccess())
                .build();
    }

    @Override
    public SuccessRespDTO resetMold(ResetMoldStepReqDTO resetMoldStepReqDTO) {
        log.info("Start reset mold to step {}", resetMoldStepReqDTO);
        Step currentStep = stepRepository.findById(resetMoldStepReqDTO.getCurrentStepId()).orElseThrow();
        Step resetStep = stepRepository.findById(resetMoldStepReqDTO.getResetToStepId()).orElseThrow();
        /* validate step status */
        purchaseOrderComponent.canUpdateOrder(currentStep.getProcedure().getPurchaseOrder());

        /* validate step status */
        stepComponent.canReportProgress(currentStep);

        /* validate reset step */
        if (!Objects.equals(currentStep.getProcedure(), resetStep.getProcedure())) {
            log.info("Validate order of step in req {}", resetMoldStepReqDTO);
            throw new BusinessException(
                    String.format(
                            "Reset step %s is not the same order with step %s",
                            resetMoldStepReqDTO.getResetToStepId(),
                            resetMoldStepReqDTO.getCurrentStepId())
            );
        }

        /* validate sequence */
        /* this rule include validate not is start step */
        if (resetStep.getSequenceNo() >= currentStep.getSequenceNo()) {
            log.info("Validate step order in req {}", resetMoldStepReqDTO);
            throw new BusinessException(
                    String.format(
                            "Reset step %s is after step %s",
                            resetMoldStepReqDTO.getResetToStepId(),
                            resetMoldStepReqDTO.getCurrentStepId())
            );
        }

        List<Step> listResetStep = new ArrayList<>();
        Step step = currentStep;

        /* loop to get all main previous step of current step until reset step */
        do {
            List<StepSequence> stepSequenceList = stepSequenceRepository.findByStepAfterId(step.getId());
            step = StepUtil.getPreMainStep(stepSequenceList);
            listResetStep.add(step);
        } while (!Objects.equals(Objects.requireNonNull(step).getId(), resetMoldStepReqDTO.getResetToStepId()));

        List<Mold> listMold = moldRepository.findAllById(resetMoldStepReqDTO.getListMoldId());
        listResetStep.forEach(stepAction -> stepComponent.resetProgress(stepAction, listMold));

        log.info("End reset mold to step");
        return SuccessRespDTO.builder()
                .message(messageConstant.getUpdateSuccess())
                .build();
    }

    @Override
    public List<MoldDTO> getListCompletedMoldInPreviousStep(Long stepId) {
        log.info("Start get list completed mold in  previous step of step id {}", stepId);
        List<StepSequence> stepSequenceList = stepSequenceRepository.findByStepAfterId(stepId);
        Step preStep = StepUtil.getPreMainStep(stepSequenceList);
        log.info("Pre step of id {} is {}", stepId, preStep);

        List<MoldProgress> moldProgressList = moldProgressRepository.findAllByStepId(preStep.getId());
        log.info("List mold progress of pre step is {}", moldProgressList);

        log.info("End get completed mold");
        return moldMapper.toDTOs(
                moldProgressList.stream()
                        .filter(moldProgress -> Boolean.TRUE.equals(moldProgress.getIsCompleted()))
                        .map(MoldProgress::getMold)
                        .collect(Collectors.toList())
        );
    }
}
