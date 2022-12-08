package com.shop.tbms.service.impl;

import com.shop.tbms.component.*;
import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.config.security.TbmsUserDetails;
import com.shop.tbms.constant.LogConstant;
import com.shop.tbms.constant.MessageConstant;
import com.shop.tbms.constant.NotificationConstant;
import com.shop.tbms.dto.mold.MoldDTO;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.noti.FBNotificationRequestDTO;
import com.shop.tbms.dto.step.ResetMoldStepReqDTO;
import com.shop.tbms.dto.step.detail.StepDTO;
import com.shop.tbms.dto.step.detail.progress.MoldElementProgressDTO;
import com.shop.tbms.dto.step.report.ReportStepReqDTO;
import com.shop.tbms.dto.step.report_issue.ReportIssueStepReqDTO;
import com.shop.tbms.dto.step.report_issue.ReportIssueToStepRespDTO;
import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.NotificationType;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.enumerate.order.OrderPaymentStatus;
import com.shop.tbms.enumerate.order.OrderStatus;
import com.shop.tbms.enumerate.step.StepStatus;
import com.shop.tbms.enumerate.step.StepType;
import com.shop.tbms.mapper.mold.MoldMapper;
import com.shop.tbms.mapper.StepMapper;
import com.shop.tbms.mapper.StepSequenceMapper;
import com.shop.tbms.mapper.progress.MoldDeliverProgressMapper;
import com.shop.tbms.mapper.progress.MoldElementProgressMapper;
import com.shop.tbms.mapper.progress.MoldProgressMapper;
import com.shop.tbms.repository.*;
import com.shop.tbms.service.NotificationService;
import com.shop.tbms.service.StepService;
import com.shop.tbms.specification.AccountSpecification;
import com.shop.tbms.specification.StepSpecification;
import com.shop.tbms.util.AuthenticationUtil;
import com.shop.tbms.util.MoldElementUtil;
import com.shop.tbms.util.NotificationUtil;
import com.shop.tbms.util.StepUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import javax.persistence.EntityNotFoundException;
import java.util.*;
import java.util.stream.Collectors;

import static com.shop.tbms.constant.AppConstant.DELETED_ID;

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
    private AccountRepository accountRepository;
    @Autowired
    private NotificationRepository notificationRepository;

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

    @Autowired
    private NotificationConstant notificationConstant;
    @Autowired
    private LogConstant logConstant;

    @Autowired
    private NotificationService notificationService;

    @Override
    public StepDTO getStep(Long stepId) {
        Step step = stepRepository.findById(stepId).orElseThrow(EntityNotFoundException::new);
        StepDTO dto = stepMapper.toDTO(step);

        /* Map progress dto */
        List<Step> preStep = StepUtil.getPreStepToChkProgress(step.getListStepAfter());
        List<Step> nextStep = StepUtil.getNextStepToChkProgress(step.getListStepBefore());
        switch (step.getReportType()) {
            case BY_MOLD:
                dto.setListMoldProgress(moldProgressMapper.toDTOs(step.getListMoldProgress()));
                if (!StepStatus.COMPLETED.equals(step.getStatus())) {
                    dto.setListMoldProgress(
                            progressComponent.setReportAvailabilityForMoldProgress(
                                    preStep,
                                    nextStep,
                                    dto.getListMoldProgress(),
                                    step)
                    );
                }
                break;
            case BY_MOLD_SEND_RECEIVE:
                dto.setListMoldDeliverProgress(moldDeliverProgressMapper.toDTOs(step.getListMoldDeliverProgress()));
                if (!StepStatus.COMPLETED.equals(step.getStatus())) {
                    dto.setListMoldDeliverProgress(
                            progressComponent.setReportAvailabilityForDeliveryProgress(
                                    preStep,
                                    nextStep,
                                    dto.getListMoldDeliverProgress(),
                                    step)
                    );
                }
                break;
            case BY_MOLD_ELEMENT:
                List<MoldElementProgressDTO> moldElementProgressDTOList = step.getProcedure().getPurchaseOrder()
                        .getListMold().stream()
                        .map(mold ->
                                MoldElementProgressDTO.builder()
                                        .moldSize(mold.getSize())
                                        .moldSizeWithType(Objects.nonNull(mold.getMoldGroup()) ? (mold.getSize() + "#" + mold.getMoldGroup().getType().name()) : mold.getSize())
                                        .percentCompleted(
                                                MoldElementUtil.calPercentComplete(
                                                        step.getListMoldGroupElementProgresses(), mold)
                                        ).listElement(
                                                moldElementProgressMapper.toDTOs(
                                                        step.getListMoldGroupElementProgresses().stream()
                                                                .filter(elementProgress ->
                                                                        mold.getId().equals(elementProgress.getMold().getId()))
                                                                .sorted(Comparator.comparing(MoldGroupElementProgress::getId))
                                                                .collect(Collectors.toList()))
                                        ).build()
                        )
                        .collect(Collectors.toList());

                dto.setListMoldElementProgress(moldElementProgressDTOList);

                if (!StepStatus.COMPLETED.equals(step.getStatus())) {
                    dto.setListMoldElementProgress(
                            progressComponent.setReportAvailabilityForMoldElementProgress(
                                    preStep,
                                    nextStep,
                                    dto.getListMoldElementProgress(),
                                    step
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
        ReportLog reportLog = new ReportLog();
        reportLog.setStep(currentStep);
        List<String> logDetail = new ArrayList<>();

        /* validate */
        /* validate order status */
        log.info("validate order status of order {}", currentOrder);
        purchaseOrderComponent.canUpdateOrder(currentOrder);

        /* update progress */
        TbmsUserDetails curUser = AuthenticationUtil.getUserDetails();
        log.info("Start update progress of step {}", currentStep);
        switch (currentStep.getReportType()) {
            case BY_MOLD:
                log.info("Start update progress of report type = BY_MOLD");
                stepComponent.updateMoldProgress(currentStep, reportStepReqDTO.getProgress(), logDetail);
                moldProgressRepository.saveAll(currentMoldProgress);
                moldProgressRepository.deleteAll(
                        currentMoldProgress.stream()
                                .filter(moldProgress -> DELETED_ID.equals(moldProgress.getId()))
                                .collect(Collectors.toList())
                );
                break;
            case BY_MOLD_ELEMENT:
                log.info("Start update progress of report type = BY_MOLD_ELEMENT");
                stepComponent.updateMoldElementProgress(currentStep, reportStepReqDTO.getProgress(), logDetail);
                moldGroupElementProgressRepository.saveAll(currentMoldElementProgress);
                moldGroupElementProgressRepository.deleteAll(
                        currentMoldElementProgress.stream()
                                .filter(moldProgress -> DELETED_ID.equals(moldProgress.getId()))
                                .collect(Collectors.toList())
                );
                break;
            case BY_MOLD_SEND_RECEIVE:
                log.info("Start update progress of report type = BY_MOLD_SEND_RECEIVE");
                stepComponent.updateMoldDeliverProgress(currentStep, reportStepReqDTO.getProgress(), logDetail);
                moldDeliverProgressRepository.saveAll(currentMoldDeliverProgress);
                moldDeliverProgressRepository.deleteAll(
                        currentMoldDeliverProgress.stream()
                                .filter(moldProgress -> DELETED_ID.equals(moldProgress.getId()))
                                .collect(Collectors.toList())
                );
                break;
            default:
        }

        log.info("Start update check list");
        stepComponent.updateChecklist(currentChecklist, reportStepReqDTO.getChecklist(), logDetail);
        checklistRepository.saveAll(currentChecklist);

        log.info("Start update step info");
        stepComponent.updateStep(currentStep, reportStepReqDTO);

        log.info("Save step new info");
        stepRepository.save(currentStep);

        log.info("Start update evidences");
        stepComponent.updateEvidence(currentStep, reportStepReqDTO.getEvidence(), reportLog);

        /* set status next step */
        log.info("Start change status for next step");
        Step nextMainStep = StepUtil.getNextMainStep(stepSequenceRepository.findByStepBeforeId(currentStep.getId()));
        if (Objects.nonNull(nextMainStep) && !StepStatus.IN_PROGRESS.equals(nextMainStep.getStatus())) {
            nextMainStep.setStatus(StepStatus.IN_PROGRESS);
            stepRepository.save(nextMainStep);
        }

        if (Boolean.TRUE.equals(currentStep.getIsEnd())) {
            if (List.of(Role.PRESIDENT, Role.SECRETARY, Role.ACCOUNTANT).contains(curUser.getRole())) {
                if (Boolean.TRUE.equals(reportStepReqDTO.getIsPaid()) && !OrderPaymentStatus.PAID.equals(currentOrder.getPaymentStatus())) {
                    currentOrder.setPaymentStatus(OrderPaymentStatus.PAID);
                    logDetail.add(String.format(logConstant.getOrderPaid(), curUser.getFullname()));
                } else if (Boolean.FALSE.equals(reportStepReqDTO.getIsPaid()) && !OrderPaymentStatus.NOT_PAID.equals(currentOrder.getPaymentStatus())) {
                    currentOrder.setPaymentStatus(OrderPaymentStatus.NOT_PAID);
                    logDetail.add(String.format(logConstant.getOrderUnpaid(), curUser.getFullname()));
                }
            }

            if (!OrderStatus.COMPLETED.equals(currentOrder.getStatus())) {
                boolean isCompleteAllMold = StepUtil.isCompleteAllMold(currentStep);

                if (isCompleteAllMold) {
                    log.info("Step {} is complete for all progress", currentStep);
                    log.info("Step is end. Start set value for end order");

                    currentOrder.setStatus(OrderStatus.COMPLETED);
                }
            }

            purchaseOrderRepository.save(currentOrder);
        }

        /* insert log */
        if (!CollectionUtils.isEmpty(logDetail)) {
            log.info("Start insert log");
            reportLogComponent.insertReportLog(currentStep, reportLog, logDetail);
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
        if (CollectionUtils.isEmpty(listStepSequence) || listStepSequence.size() == 1) return new ArrayList<>();

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
                        "Cannot found valid next step id with step id = " + reportIssueStepReqDTO.getChangeToStepId()
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

            if (StepType.FIXING.equals(resetStep.getType())) {
                switch (currentStep.getReportType()) {
                    case BY_MOLD: {
                        List<MoldProgress> progressList = currentStep.getListMoldProgress().stream()
                                .filter(progress -> reportIssueStepReqDTO.getListMoldId().contains(progress.getMold().getId()))
                                .collect(Collectors.toList());

                        progressList.forEach(progress -> progress.setIsCompleted(Boolean.FALSE));
                        moldProgressRepository.saveAll(progressList);
                    }
                    break;
                    case BY_MOLD_SEND_RECEIVE: {
                        List<MoldDeliverProgress> progressList = currentStep.getListMoldDeliverProgress().stream()
                                .filter(progress -> reportIssueStepReqDTO.getListMoldId().contains(progress.getMold().getId()))
                                .collect(Collectors.toList());

                        progressList.forEach(progress -> progress.setIsCompleted(Boolean.FALSE));
                        moldDeliverProgressRepository.saveAll(progressList);
                    }
                    break;
                    case BY_MOLD_ELEMENT: {
                        List<MoldGroupElementProgress> progressList = currentStep.getListMoldGroupElementProgresses().stream()
                                .filter(progress -> reportIssueStepReqDTO.getListMoldId().contains(progress.getMold().getId()))
                                .collect(Collectors.toList());

                        progressList.forEach(progress -> progress.setIsCompleted(Boolean.FALSE));
                        moldGroupElementProgressRepository.saveAll(progressList);
                    }
                    break;
                    default:
                }

                progressComponent.resetProgress(resetStep, listReportedMold, false);
            } else {
                Step endStep = StepUtil.getEndStep(currentStep.getProcedure().getPurchaseOrder());
                resetMoldProgressToStep(endStep, resetStep.getId(), reportIssueStepReqDTO.getListMoldId());
            }
        }

        return SuccessRespDTO.builder()
                .message(MessageConstant.UPDATE_SUCCESS)
                .build();
    }

    @Override
    public SuccessRespDTO resetMold(ResetMoldStepReqDTO resetMoldStepReqDTO) {
        log.info("Start reset mold to step {}", resetMoldStepReqDTO);
        Step resetStep = stepRepository.findById(resetMoldStepReqDTO.getCurrentStepId()).orElseThrow();
        /* validate step status */
        purchaseOrderComponent.canUpdateOrder(resetStep.getProcedure().getPurchaseOrder());

        Step endStep = StepUtil.getEndStep(resetStep.getProcedure().getPurchaseOrder());
        resetMoldProgressToStep(endStep, resetStep.getId(), resetMoldStepReqDTO.getListMoldId());
        log.info("End reset mold to step");
        return SuccessRespDTO.builder()
                .message(MessageConstant.UPDATE_SUCCESS)
                .build();
    }

    private void resetMoldProgressToStep(Step endStep, Long resetToStepId, List<Long> listMoldId) {
        /* loop to get all main previous step of current step until reset step */
        List<Step> listResetStep = new ArrayList<>();
        listResetStep.add(endStep);
        Step step = endStep;

        do {
            List<StepSequence> stepSequenceList = stepSequenceRepository.findByStepAfterId(step.getId());
            step = StepUtil.getPreMainStep(stepSequenceList);
            listResetStep.add(step);
        } while (!Objects.equals(Objects.requireNonNull(step).getId(), resetToStepId));

        List<Mold> listMold = moldRepository.findAllById(listMoldId);
        listResetStep.forEach(stepAction -> progressComponent.resetProgress(stepAction, listMold, false));
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

    @Override
    public void notiStepNearlyLate() {
        List<Step> stepListNearlyLate = stepRepository.findAll(StepSpecification.getStepNearlyLate());
        log.info("List step nearly late {}", stepListNearlyLate);

        List<TbmsNotification> notificationList = new ArrayList<>();
        stepListNearlyLate.stream().map(step -> {
                    List<Account> receiverList = accountRepository
                            .findAll(AccountSpecification.genGetListByRoleAndPosition(
                                    List.of(Role.EMPLOYEE),
                                    List.of(step.getCode()))
                            );

                    return receiverList.stream().map(receiver -> {
                        FBNotificationRequestDTO fbNotificationRequestDTO = NotificationUtil.genNotiStepNearlyLate(step, notificationConstant, receiver.getUsername());
                        notificationList.add(
                                NotificationUtil.genEntityNotification(
                                        fbNotificationRequestDTO,
                                        step,
                                        NotificationType.STEP_NEARLY_LATE)
                        );

                        return fbNotificationRequestDTO;
                    }).collect(Collectors.toList());
                }).flatMap(Collection::stream)
                .forEach(fbNotificationRequestDTO -> {
                    try {
                        notificationService.sendPnsToTopic(fbNotificationRequestDTO);
                    } catch (Exception e) {
                        log.error("Error while send notification {}", fbNotificationRequestDTO);
                    }
                });

        notificationRepository.saveAll(notificationList);
    }

    @Override
    public void notiStepLate() {
        List<Step> stepListLate = stepRepository.findAll(StepSpecification.getStepLate());
        log.info("List step late {}", stepListLate);

        List<TbmsNotification> notificationList = new ArrayList<>();

        stepListLate.stream().map(step -> {
                    List<Account> receiverList = accountRepository
                            .findAll(AccountSpecification.genGetListByRoleAndPosition(
                                    List.of(Role.EMPLOYEE),
                                    List.of(step.getCode()))
                            );

                    return receiverList.stream().map(receiver -> {
                        FBNotificationRequestDTO fbNotificationRequestDTO = NotificationUtil.genNotiStepLate(step, notificationConstant, receiver.getUsername());
                        notificationList.add(
                                NotificationUtil.genEntityNotification(
                                        fbNotificationRequestDTO,
                                        step,
                                        NotificationType.STEP_LATE)
                        );

                        return fbNotificationRequestDTO;
                    }).collect(Collectors.toList());
                }).flatMap(Collection::stream)
                .forEach(fbNotificationRequestDTO -> {
                    try {
                        notificationService.sendPnsToTopic(fbNotificationRequestDTO);
                    } catch (Exception e) {
                        log.error("Error while send notification {}", fbNotificationRequestDTO);
                    }
                });

        notificationRepository.saveAll(notificationList);
    }
}
