package com.shop.tbms.service.impl;

import com.shop.tbms.component.*;
import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.constant.MessageConstant;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.order.OrderCreateReqDTO;
import com.shop.tbms.dto.order.OrderDetailRespDTO;
import com.shop.tbms.dto.order.OrderFilterReqDTO;
import com.shop.tbms.dto.order.OrderListRespDTO;
import com.shop.tbms.dto.step.UpdateExpectedCompleteReqDTO;
import com.shop.tbms.dto.step.UpdateExpectedCompleteRespDTO;
import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.OrderStatus;
import com.shop.tbms.enumerate.StepStatus;
import com.shop.tbms.mapper.order.PurchaseOrderDetailMapper;
import com.shop.tbms.mapper.order.PurchaseOrderListMapper;
import com.shop.tbms.mapper.order.PurchaseOrderMapper;
import com.shop.tbms.repository.*;
import com.shop.tbms.service.PurchaseOrderService;
import com.shop.tbms.util.MoldProgressUtil;
import com.shop.tbms.util.TemplateUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityNotFoundException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
public class PurchaseOrderServiceImpl implements PurchaseOrderService {
    /* Constant */
    @Autowired
    private MessageConstant messageConstant;

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
    private MoldProgressRepository moldProgressRepository;
    @Autowired
    private StepRepository stepRepository;

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

        /* Process begin step */
        Step beginningStep = procedure.getListStep().stream()
                .filter(step -> Boolean.TRUE.equals(step.getIsStart()))
                .findFirst()
                .orElseThrow(() -> new BusinessException("Not found start step"));
        beginningStep.setStatus(StepStatus.IN_PROGRESS);

        /* Generate process of begin step */
        List<MoldProgress> listMoldProgress = MoldProgressUtil.generateProcess(listMold, beginningStep);

        /* Insert data MoldProgress */
        moldProgressRepository.saveAll(listMoldProgress);

        /* Update status of Step */
        stepRepository.save(beginningStep);

        return SuccessRespDTO.builder()
                .message(messageConstant.getCreateSuccess())
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
        Specification<PurchaseOrder> specification = purchaseOrderComponent.buildSpecForList(filterReqDTO);
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
}
