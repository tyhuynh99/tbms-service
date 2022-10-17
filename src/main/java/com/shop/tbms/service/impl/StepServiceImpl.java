package com.shop.tbms.service.impl;

import com.shop.tbms.component.*;
import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.constant.MessageConstant;
import com.shop.tbms.dto.mold.MoldDTO;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.step.ResetMoldStepReqDTO;
import com.shop.tbms.dto.step.detail.StepDTO;
import com.shop.tbms.dto.step.detail.progress.MoldElementProgressDTO;
import com.shop.tbms.dto.step.report.ReportStepReqDTO;
import com.shop.tbms.dto.step.report_issue.ReportIssueStepReqDTO;
import com.shop.tbms.dto.step.report_issue.ReportIssueToStepRespDTO;
import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.order.OrderPaymentStatus;
import com.shop.tbms.enumerate.order.OrderStatus;
import com.shop.tbms.enumerate.step.StepStatus;
import com.shop.tbms.mapper.mold.MoldMapper;
import com.shop.tbms.mapper.StepMapper;
import com.shop.tbms.mapper.StepSequenceMapper;
import com.shop.tbms.mapper.progress.MoldDeliverProgressMapper;
import com.shop.tbms.mapper.progress.MoldElementProgressMapper;
import com.shop.tbms.mapper.progress.MoldProgressMapper;
import com.shop.tbms.repository.*;
import com.shop.tbms.service.StepService;
import com.shop.tbms.util.MoldElementUtil;
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
    private MoldProgressMapper moldProgressMapper;
    @Autowired
    private MoldDeliverProgressMapper moldDeliverProgressMapper;
    @Autowired
    private MoldElementProgressMapper moldElementProgressMapper;

    @Autowired
    private StepRepository stepRepository;
    @Autowired
    private MoldProgressRepository moldProgressRepository;
    @Autowired
    private MoldDeliverProgressRepository moldDeliverProgressRepository;
    @Autowired
    private MoldGroupElementProgressRepository moldGroupElementProgressRepository;
    @Autowired
    private ChecklistRepository checklistRepository;
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
    private ProgressComponent progressComponent;

    @Override
    public StepDTO getStep(Long stepId) {
        Step step = stepRepository.findById(stepId).orElseThrow(EntityNotFoundException::new);
        StepDTO dto = stepMapper.toDTO(step);

        boolean notStartStep = Boolean.FALSE.equals(step.getIsStart());

        /* Map progress dto */
        switch (step.getReportType()) {
            case BY_MOLD:
                dto.setListMoldProgress(moldProgressMapper.toDTOs(step.getListMoldProgress()));
                if (notStartStep) {
                    dto.setListMoldProgress(
                            progressComponent.setCanCheckForMoldProgress(
                                    StepUtil.getPreMainStep(step.getListStepBefore()),
                                    dto.getListMoldProgress()
                            )
                    );
                }
                break;
            case BY_MOLD_SEND_RECEIVE:
                dto.setListMoldDeliverProgress(moldDeliverProgressMapper.toDTOs(step.getListMoldDeliverProgress()));
                if (notStartStep) {
                    dto.setListMoldDeliverProgress(
                            progressComponent.setCanCheckForDeliveryProgress(
                                    StepUtil.getPreMainStep(step.getListStepBefore()),
                                    dto.getListMoldDeliverProgress()
                            )
                    );
                }
                break;
            case BY_MOLD_ELEMENT:
                List<MoldElementProgressDTO> moldElementProgressDTOList = step.getProcedure().getPurchaseOrder()
                        .getListMold().stream()
                        .map(mold ->
                                MoldElementProgressDTO.builder()
                                        .moldSize(mold.getSize() + "#" + mold.getMoldGroup().getType().name())
                                        .percentCompleted(
                                                MoldElementUtil.calPercentComplete(
                                                        step.getListMoldGroupElementProgresses(), mold)
                                        ).listElement(
                                                moldElementProgressMapper.toDTOs(
                                                        step.getListMoldGroupElementProgresses().stream()
                                                                .filter(elementProgress ->
                                                                        mold.getId().equals(elementProgress.getMold().getId()))
                                                                .collect(Collectors.toList()))
                                        ).build()
                        )
                        .collect(Collectors.toList());

                dto.setListMoldElementProgress(moldElementProgressDTOList);

                if (notStartStep) {
                    dto.setListMoldElementProgress(
                            progressComponent.setCanCheckForMoldElementProgress(
                                    StepUtil.getPreMainStep(step.getListStepBefore()),
                                    dto.getListMoldElementProgress()
                            )
                    );
                }
                break;
            default:
        }

        return dto;
    }

    @Override
    public SuccessRespDTO reportStepProgress(ReportStepReqDTO reportStepReqDTO) {
        log.info("Begin report step progress {}", reportStepReqDTO);
        Step currentStep = stepRepository.findById(reportStepReqDTO.getStepId()).orElseThrow(EntityNotFoundException::new);
        PurchaseOrder currentOrder = currentStep.getProcedure().getPurchaseOrder();
        List<MoldProgress> currentMoldProgress = currentStep.getListMoldProgress();
        List<MoldDeliverProgress> currentMoldDeliverProgress = currentStep.getListMoldDeliverProgress();
        List<MoldGroupElementProgress> currentMoldElementProgress = currentStep.getListMoldGroupElementProgresses();
        List<Checklist> currentChecklist = currentStep.getListChecklist();

        /* validate */
        /* validate order status */
        log.info("validate order status of order {}", currentOrder);
        purchaseOrderComponent.canUpdateOrder(currentOrder);

        /* validate step status */
        log.info("validate step status {}", currentStep);
        stepComponent.canReportProgress(currentStep);

        /* update progress */
        log.info("Start update progress of step {}", currentStep);
        switch (currentStep.getReportType()) {
            case BY_MOLD:
                log.info("Start update progress of report type = BY_MOLD");
                stepComponent.updateMoldProgress(currentStep, reportStepReqDTO.getProgress());
                moldProgressRepository.saveAll(currentMoldProgress);
                break;
            case BY_MOLD_ELEMENT:
                log.info("Start update progress of report type = BY_MOLD_ELEMENT");
                stepComponent.updateMoldElementProgress(currentStep, reportStepReqDTO.getProgress());
                moldGroupElementProgressRepository.saveAll(currentMoldElementProgress);
                break;
            case BY_MOLD_SEND_RECEIVE:
                log.info("Start update progress of report type = BY_MOLD_SEND_RECEIVE");
                stepComponent.updateMoldDeliverProgress(currentStep, reportStepReqDTO.getProgress());
                moldDeliverProgressRepository.saveAll(currentMoldDeliverProgress);
                break;
            default:
        }

        log.info("Start update check list");
        stepComponent.updateChecklist(currentChecklist, reportStepReqDTO.getChecklist());
        checklistRepository.saveAll(currentChecklist);

        log.info("Start update evidences");
        stepComponent.updateEvidence(currentStep, reportStepReqDTO.getEvidence());

        log.info("Start update step info");
        stepComponent.updateStep(currentStep, reportStepReqDTO);
        stepComponent.updateStepStatus(currentStep, currentMoldProgress);

        log.info("Save step new info");
        stepRepository.save(currentStep);

        /* set status next step */
        log.info("Start change status for next step");
        Step nextMainStep = StepUtil.getNextMainStep(stepSequenceRepository.findByStepBeforeId(currentStep.getId()));
        if (!StepStatus.IN_PROGRESS.equals(nextMainStep.getStatus())) {
            nextMainStep.setStatus(StepStatus.IN_PROGRESS);
            stepRepository.save(nextMainStep);
        }

        /* insert log */
        log.info("Start insert log");
        reportLogComponent.insertReportLog(currentStep, reportStepReqDTO);

        if (Boolean.TRUE.equals(currentStep.getIsEnd())) {
            if (Boolean.TRUE.equals(reportStepReqDTO.getIsPaid())) {
                currentOrder.setPaymentStatus(OrderPaymentStatus.PAID);
            } else {
                currentOrder.setPaymentStatus(OrderPaymentStatus.NOT_PAID);
            }

            boolean isCompleteAllMold = StepUtil.isCompleteAllMold(currentStep);

            if (isCompleteAllMold) {
                log.info("Step {} is complete for all progress", currentStep);
                log.info("Step is end. Start set value for end order");

                currentOrder.setStatus(OrderStatus.COMPLETED);
            }

            purchaseOrderRepository.save(currentOrder);
        }

        log.info("End report step progress");

        return SuccessRespDTO.builder()
                .message(MessageConstant.UPDATE_SUCCESS)
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
            progressComponent.resetProgress(resetStep, listReportedMold);
        }

        /* insert log */
        reportLogComponent.insertReportLog(currentStep, reportIssueStepReqDTO);

        return SuccessRespDTO.builder()
                .message(MessageConstant.UPDATE_SUCCESS)
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
        listResetStep.forEach(stepAction -> progressComponent.resetProgress(stepAction, listMold));

        log.info("End reset mold to step");
        return SuccessRespDTO.builder()
                .message(MessageConstant.UPDATE_SUCCESS)
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
