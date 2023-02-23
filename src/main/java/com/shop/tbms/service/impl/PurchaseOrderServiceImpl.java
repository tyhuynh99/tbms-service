package com.shop.tbms.service.impl;

import com.shop.tbms.component.ChecklistComponent;
import com.shop.tbms.component.MoldComponent;
import com.shop.tbms.component.ProcedureComponent;
import com.shop.tbms.component.ProgressComponent;
import com.shop.tbms.component.PurchaseOrderComponent;
import com.shop.tbms.component.StepComponent;
import com.shop.tbms.component.StepSequenceComponent;
import com.shop.tbms.constant.MessageConstant;
import com.shop.tbms.constant.NotificationConstant;
import com.shop.tbms.constant.StepConstant;
import com.shop.tbms.dto.FileDTO;
import com.shop.tbms.dto.ListWrapperDTO;
import com.shop.tbms.dto.PDFDto;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.noti.FBNotificationRequestDTO;
import com.shop.tbms.dto.order.OrderCreateReqDTO;
import com.shop.tbms.dto.order.OrderDetailRespDTO;
import com.shop.tbms.dto.order.OrderFilterReqDTO;
import com.shop.tbms.dto.order.OrderListRespDTO;
import com.shop.tbms.dto.order.OrderUpdateReqDTO;
import com.shop.tbms.dto.step.upd_expect_date.UpdateExpectedCompleteReqDTO;
import com.shop.tbms.dto.step.upd_expect_date.UpdateExpectedCompleteRespDTO;
import com.shop.tbms.entity.Account;
import com.shop.tbms.entity.Checklist;
import com.shop.tbms.entity.FilePDF;
import com.shop.tbms.entity.Mold;
import com.shop.tbms.entity.Procedure;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.entity.Step;
import com.shop.tbms.entity.StepSequence;
import com.shop.tbms.entity.TbmsNotification;
import com.shop.tbms.entity.TemplateProcedure;
import com.shop.tbms.enumerate.NotificationType;
import com.shop.tbms.enumerate.Role;
import com.shop.tbms.enumerate.order.OrderStatus;
import com.shop.tbms.enumerate.step.ReportType;
import com.shop.tbms.enumerate.step.StepStatus;
import com.shop.tbms.enumerate.step.StepType;
import com.shop.tbms.mapper.order.PurchaseOrderDetailMapper;
import com.shop.tbms.mapper.order.PurchaseOrderListMapper;
import com.shop.tbms.mapper.order.PurchaseOrderMapper;
import com.shop.tbms.repository.AccountRepository;
import com.shop.tbms.repository.ChecklistRepository;
import com.shop.tbms.repository.FilePDFRepository;
import com.shop.tbms.repository.NotificationRepository;
import com.shop.tbms.repository.PurchaseOrderRepository;
import com.shop.tbms.repository.StepRepository;
import com.shop.tbms.repository.StepSequenceRepository;
import com.shop.tbms.service.FileService;
import com.shop.tbms.service.NotificationService;
import com.shop.tbms.service.PurchaseOrderService;
import com.shop.tbms.specification.AccountSpecification;
import com.shop.tbms.specification.PurchaseOrderSpecification;
import com.shop.tbms.util.AuthenticationUtil;
import com.shop.tbms.util.FileUtil;
import com.shop.tbms.util.NotificationUtil;
import com.shop.tbms.util.OrderUtil;
import com.shop.tbms.util.StepConditionUtil;
import com.shop.tbms.util.TemplateUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import javax.persistence.EntityNotFoundException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
@Slf4j
public class PurchaseOrderServiceImpl implements PurchaseOrderService {
    /* Component */
    @Autowired
    private ProcedureComponent procedureComponent;
    @Autowired
    private StepSequenceComponent stepSequenceComponent;
    @Autowired
    private MoldComponent moldComponent;
    @Autowired
    private ChecklistComponent checklistComponent;
    @Autowired
    private StepComponent stepComponent;
    @Autowired
    private PurchaseOrderComponent purchaseOrderComponent;
    @Autowired
    private ProgressComponent progressComponent;

    /* Service */
    @Autowired
    private NotificationService notificationService;

    /* Mapper */
    @Autowired
    private PurchaseOrderMapper purchaseOrderMapper;
    @Autowired
    private PurchaseOrderDetailMapper purchaseOrderDetailMapper;
    @Autowired
    private PurchaseOrderListMapper purchaseOrderListMapper;

    /* Repository */
    @Autowired
    private PurchaseOrderRepository purchaseOrderRepository;
    @Autowired
    private StepSequenceRepository stepSequenceRepository;
    @Autowired
    private ChecklistRepository checklistRepository;
    @Autowired
    private StepRepository stepRepository;
    @Autowired
    private AccountRepository accountRepository;
    @Autowired
    private NotificationRepository notificationRepository;

    /* Constant */
    @Autowired
    private StepConstant stepConstant;
    @Autowired
    private NotificationConstant notificationConstant;

    @Autowired
    private FileService fileService;

    @Autowired
    private FilePDFRepository filePDFRepository;

    @Override
    public SuccessRespDTO createOrder(OrderCreateReqDTO orderCreateReqDTO) {
        /* Generate PurchaseOrder entity */
        PurchaseOrder purchaseOrder = purchaseOrderMapper.toOrderEntity(orderCreateReqDTO);
        purchaseOrder.setStatus(OrderStatus.IN_PROGRESS);

        /* Get TemplateProcedure */
        TemplateProcedure templateProcedure = procedureComponent.getTemplateProcedure(orderCreateReqDTO.getProcedureCode());

        /* Generate Procedure entity with Steps */
        Procedure procedure = procedureComponent.generateProcedureFromTemplate(templateProcedure);

        /* Set relation of order & procedure */
        purchaseOrder.setProcedure(procedure);
        procedure.setPurchaseOrder(purchaseOrder);

        /* Generate Mold with relation with Order */
        List<Mold> listMold = moldComponent.generateMoldEntity(orderCreateReqDTO.getListSize(), purchaseOrder);

        /* Set relation from Order to Mold */
        purchaseOrder.setListMold(listMold);

        /* Insert data procedure & step */
        purchaseOrderRepository.save(purchaseOrder);

        /* Generate StepSequence from template */
        List<StepSequence> listStepSequence = stepSequenceComponent
                .generateStepSequence(
                        purchaseOrder.getProcedure().getListStep(),
                        TemplateUtil.getTemplateStepSequence(templateProcedure));

        /* Insert data StepSequence */
        stepSequenceRepository.saveAll(listStepSequence);

        /* Generate Checklist from template */
        List<Checklist> listChecklist = checklistComponent
                .generateChecklist(
                        purchaseOrder.getProcedure().getListStep(),
                        TemplateUtil.getTemplateChecklist(templateProcedure));

        /* Insert data Checklist */
        checklistRepository.saveAll(listChecklist);

        /* Process steps */
        procedure.getListStep().forEach(step -> {
            /* Process steps */
            if (Boolean.TRUE.equals(step.getIsStart())) step.setStatus(StepStatus.IN_PROGRESS);

            /* Generate mold progress */
            if (!StepType.FIXING.equals(step.getType())
                    && !ReportType.BY_MOLD_ELEMENT.equals(step.getReportType())
                    && !StepConditionUtil.isStepHasConditionProgress(step)) {
                progressComponent.generateProgressForStep(step);
            }
        });

        /* Update status of Step */
        stepRepository.saveAll(procedure.getListStep());

        return SuccessRespDTO.builder()
                .message(MessageConstant.CREATE_SUCCESS)
                .build();
    }

    /*
     * Update normal information
     * Update mold:
     * * If remove mold -> update progress of all step
     * * If add new mold -> update progress of all step
     * */
    @Override
    public SuccessRespDTO updateOrder(OrderUpdateReqDTO orderUpdateReqDTO) {
        PurchaseOrder currentOrder = purchaseOrderRepository.findById(orderUpdateReqDTO.getOrderId())
                .orElseThrow(EntityNotFoundException::new);

        /* validate current order */
        purchaseOrderComponent.canUpdateOrder(currentOrder);

        /* update normal information */
        purchaseOrderMapper.partialUpdateOrderEntity(currentOrder, orderUpdateReqDTO);
        purchaseOrderRepository.save(currentOrder);

        /* update mold & progress of mold */
        moldComponent.updateListMoldInOrder(currentOrder, orderUpdateReqDTO);

        return SuccessRespDTO.builder()
                .message(MessageConstant.UPDATE_SUCCESS)
                .build();
    }

    @Override
    public SuccessRespDTO deleteOrder(Long orderId) {
        PurchaseOrder order = purchaseOrderRepository.findById(orderId).orElseThrow(EntityNotFoundException::new);
        /* validate */
        OrderUtil.validateDeletedOrder(order);

        order.setIsDeleted(Boolean.TRUE);
        order.setDeleteBy(AuthenticationUtil.getUserDetails().getUsername());
        order.setDeleteDate(LocalDateTime.now());
        purchaseOrderRepository.save(order);

        return SuccessRespDTO.builder()
                .message(MessageConstant.DELETE_SUCCESS)
                .build();
    }

    @Override
    public OrderDetailRespDTO getOrderById(Long orderId) {
        PurchaseOrder order = purchaseOrderRepository.findById(orderId).orElseThrow(EntityNotFoundException::new);

        OrderDetailRespDTO respDTO = purchaseOrderDetailMapper.fromEntityToDetailDTO(order);

        respDTO.setListStep(stepComponent.setPercentProgress(respDTO.getListStep(), order.getProcedure().getListStep()));

        Collections.sort(respDTO.getListStep());

        return respDTO;
    }

    @Override
    public Page<OrderListRespDTO> getListOrder(OrderFilterReqDTO filterReqDTO, Pageable pageable) {
        Specification<PurchaseOrder> specification = PurchaseOrderSpecification.buildSpecForList(filterReqDTO);
        return purchaseOrderRepository.findAll(specification, pageable).map(purchaseOrderListMapper::toListResp);
    }

    @Override
    public List<UpdateExpectedCompleteRespDTO> updateStepExpectedComplete(List<UpdateExpectedCompleteReqDTO> listReqDTO) {
        List<Step> listStep = stepRepository.findAllById(
                listReqDTO.stream()
                        .map(UpdateExpectedCompleteReqDTO::getStepId)
                        .collect(Collectors.toList()));

        /* Validate request */
        stepComponent.validateUpdateExpectedCompleteData(listReqDTO, listStep);

        List<Long> listUpdatedId = new ArrayList<>();

        for (Step curStep : listStep) {
            LocalDate updateExpectDate = listReqDTO.stream()
                    .filter(reqDTO -> curStep.getId().equals(reqDTO.getStepId()))
                    .map(UpdateExpectedCompleteReqDTO::getExpectedCompleteDate)
                    .findFirst()
                    .orElse(null);

            if (Objects.nonNull(updateExpectDate)) {
                curStep.setExpectedCompleteDate(updateExpectDate);
                listUpdatedId.add(curStep.getId());
            }
        }

        stepRepository.saveAll(listStep);

        return listUpdatedId.stream()
                .map(updatedId ->
                        UpdateExpectedCompleteRespDTO
                                .builder()
                                .stepId(updatedId)
                                .build())
                .collect(Collectors.toList());
    }

    @Override
    public void checkLateOrder() {
        List<PurchaseOrder> listLateOrder = purchaseOrderRepository.findAll(PurchaseOrderSpecification.getListLateOrderToday());

        listLateOrder.forEach(order -> order.setIsLate(Boolean.TRUE));

        listLateOrder.addAll(purchaseOrderRepository.findAll(PurchaseOrderSpecification.getListLateOrderOtherDay()));

        /* send noti all president and secretary */
        List<Account> listReceiver = accountRepository.findAll(
                AccountSpecification.genGetListByRole(List.of(Role.PRESIDENT, Role.SECRETARY)));

        log.info("Start send noti of overdue order to {} ", listReceiver.stream().map(Account::getUsername).collect(Collectors.toList()));

        List<FBNotificationRequestDTO> listFbNoti = listReceiver.stream()
                .map(receiver ->
                        listLateOrder.stream()
                                .map(order -> NotificationUtil.genNotiOrderOverdue(
                                        order,
                                        notificationConstant,
                                        receiver.getUsername()))
                                .collect(Collectors.toList()))
                .flatMap(List::stream)
                .collect(Collectors.toList());

        listFbNoti.forEach(requestDTO -> {
            try {
                notificationService.sendPnsToTopic(requestDTO);
            } catch (Exception e) {
                log.error("Error while send notification {}", requestDTO);
            }
        });

        List<TbmsNotification> notificationList = listFbNoti.stream()
                .map(requestDTO -> NotificationUtil.genEntityNotification(
                        requestDTO,
                        NotificationType.ORDER_OVERDUE))
                .collect(Collectors.toList());

        log.info("Complete send noti of overdue order");
        purchaseOrderRepository.saveAll(listLateOrder);
        notificationRepository.saveAll(notificationList);
    }

    @Override
    public void notiNearlyDueOrder() {
        List<PurchaseOrder> orderNearlyDue = purchaseOrderRepository.findAll(PurchaseOrderSpecification.getListNearlyDueOrder());
        log.info("Get list of order nearly due get {}", orderNearlyDue);

        /* send noti all president and secretary */
        List<Account> listReceiver = accountRepository.findAll(
                AccountSpecification.genGetListByRole(List.of(Role.PRESIDENT, Role.SECRETARY)));

        log.info("Start send noti of nearly due order to {} ", listReceiver.stream().map(Account::getUsername).collect(Collectors.toList()));

        List<FBNotificationRequestDTO> listFbNoti = listReceiver.stream()
                .map(receiver ->
                        orderNearlyDue.stream()
                                .map(order -> NotificationUtil.genNotiOrderNearlyDue(
                                        order,
                                        notificationConstant,
                                        receiver.getUsername()))
                                .collect(Collectors.toList()))
                .flatMap(List::stream)
                .collect(Collectors.toList());

        listFbNoti.forEach(requestDTO -> {
            try {
                notificationService.sendPnsToTopic(requestDTO);
            } catch (Exception e) {
                log.error("Error while send notification {}", requestDTO);
            }
        });

        List<TbmsNotification> notificationList = listFbNoti.stream()
                .map(requestDTO -> NotificationUtil.genEntityNotification(
                        requestDTO,
                        NotificationType.ORDER_NEARLY_DUE)
                )
                .collect(Collectors.toList());

        log.info("Complete send noti for nearly due order");
        notificationRepository.saveAll(notificationList);
    }

    @Override
    public List<FileDTO> uploadPDF(Long orderId, MultipartFile[] files) throws Exception {
        PurchaseOrder order = purchaseOrderRepository.findById(orderId).orElseThrow(EntityNotFoundException::new);
        List<FileDTO> fileDTOList = fileService.uploadPDF(orderId, files);
        List<FilePDF> listPDF = fileDTOList.stream().map(x -> FilePDF.builder()
                .filename(x.getFilename())
                .url(x.getUrl())
                .purchaseOrder(order)
                .isDelete(false)
                .build()).collect(Collectors.toList());
        filePDFRepository.saveAll(listPDF);
        return fileDTOList;
    }

    @Override
    public ListWrapperDTO<PDFDto> getPDF(Long orderId) {
        List<FilePDF> filePDFList = filePDFRepository.findAllByPurchaseOrderId(orderId);
        ListWrapperDTO<PDFDto> result = new ListWrapperDTO();
        result.setData(filePDFList.stream().map(x -> PDFDto.builder()
                        .id(x.getId())
                        .filename(x.getFilename())
                        .url(x.getUrl())
                        .build())
                .collect(Collectors.toList()));
        return result;
    }

    @Override
    public SuccessRespDTO deletePDF(Long pdfId) {
        try {
            FilePDF pdf = filePDFRepository.findById(pdfId).orElseThrow(Exception::new);
            String destination = FileUtil.generateDestination(pdf.getPurchaseOrder().getId(), pdf.getFilename());
            filePDFRepository.deleteById(pdfId);
            fileService.deleteFile(destination);
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        return SuccessRespDTO.builder()
                .message(MessageConstant.DELETE_SUCCESS)
                .build();
    }
}
