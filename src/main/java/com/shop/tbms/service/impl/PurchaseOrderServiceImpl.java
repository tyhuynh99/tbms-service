package com.shop.tbms.service.impl;

import com.shop.tbms.component.ChecklistComponent;
import com.shop.tbms.component.MoldComponent;
import com.shop.tbms.component.ProcedureComponent;
import com.shop.tbms.component.StepSequenceComponent;
import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.constant.MessageConstant;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.order.OrderCreateReqDTO;
import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.StepStatus;
import com.shop.tbms.mapper.order.PurchaseOrderMapper;
import com.shop.tbms.repository.*;
import com.shop.tbms.service.PurchaseOrderService;
import com.shop.tbms.util.MoldProgressUtil;
import com.shop.tbms.util.TemplateUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

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

    /* Mapper */
    @Autowired
    private PurchaseOrderMapper purchaseOrderMapper;

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
}
