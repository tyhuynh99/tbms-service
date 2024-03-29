package com.shop.tbms.service.impl;

import com.shop.tbms.component.ProgressComponent;
import com.shop.tbms.component.ProgressMoldGroupComponent;
import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.constant.MessageConstant;
import com.shop.tbms.constant.StepConstant;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.mold.*;
import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.order.OrderStatus;
import com.shop.tbms.mapper.mold.MoldElementTemplateMapper;
import com.shop.tbms.mapper.mold.MoldGroupDetailMapper;
import com.shop.tbms.repository.*;
import com.shop.tbms.service.MoldService;
import com.shop.tbms.util.ProgressUtil;
import com.shop.tbms.util.StepConditionUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import javax.persistence.EntityNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
@Transactional
@Slf4j
public class MoldServiceImpl implements MoldService {
    @Autowired
    private TemplateMoldElementRepository templateMoldElementRepository;
    @Autowired
    private PurchaseOrderRepository orderRepository;
    @Autowired
    private MoldGroupRepository moldGroupRepository;
    @Autowired
    private MoldProgressRepository moldProgressRepository;

    @Autowired
    private MoldElementTemplateMapper moldElementTemplateMapper;
    @Autowired
    private MoldGroupDetailMapper moldGroupDetailMapper;

    @Autowired
    private ProgressComponent progressComponent;
    @Autowired
    private ProgressMoldGroupComponent progressMoldGroupComponent;

    @Autowired
    private StepConstant stepConstant;

    @Override
    public List<MoldElementTemplateDTO> getListElementTemplate() {
        return moldElementTemplateMapper.toDTOs(templateMoldElementRepository.findAll());
    }

    @Override
    public SuccessRespDTO saveMoldGroup(MoldGroupReqDTO reqDTO) {
        /* Get current order */
        PurchaseOrder order = orderRepository.findById(reqDTO.getOrderId()).orElseThrow();

        if (!OrderStatus.IN_PROGRESS.equals(order.getStatus())) {
            throw new BusinessException("Order status cannot update mold group");
        }

        if (Objects.nonNull(reqDTO.getMoldGroup().getId())) {
            /* update */
            updateMoldGroup(order, reqDTO);
        } else {
            /* create */
            createNewMoldGroup(order, reqDTO);
        }

        /* delete group has no mold */
        deleteGroupHasNoMold(order);

        return SuccessRespDTO.builder().message(MessageConstant.UPDATE_SUCCESS).build();
    }

    private void createNewMoldGroup(PurchaseOrder order, MoldGroupReqDTO reqDTO) {
        List<Long> listReqMoldId = reqDTO.getMoldGroup().getMoldIdList();
        List<Mold> listUpdateMold = order.getListMold().stream()
                .filter(mold -> listReqMoldId.contains(mold.getId()))
                .collect(Collectors.toList());

        if (CollectionUtils.isEmpty(listUpdateMold)) {
            throw new BusinessException("Not found any mold id " +  reqDTO.getMoldGroup().getMoldIdList() + " in order " + order.getCode());
        }

        MoldGroup moldGroup = new MoldGroup();
        moldGroup.setPurchaseOrder(order);
        moldGroup.setListMold(listUpdateMold);

        /* Validate current mold group */
        listUpdateMold.forEach(mold -> {
            if (Objects.nonNull(mold.getMoldGroup())) {
                throw new BusinessException(
                        "Mold " + mold.getSize() + " is currently in mold group "
                                + mold.getMoldGroup().getId() + " " + mold.getMoldGroup().getType());
            }

            mold.setMoldGroup(moldGroup);
        });
        moldGroupDetailMapper.partialUpdate(moldGroup, reqDTO.getMoldGroup());

        List<MoldGroupElement> moldGroupElementList = new ArrayList<>();
        reqDTO.getMoldGroup().getMoldElementList().forEach(moldElementDTO -> {
            MoldGroupElement moldGroupElement = new MoldGroupElement();
            moldGroupElement.setMoldGroup(moldGroup);
            moldGroupElement.setChecked(moldElementDTO.isChecked());
            moldGroupElement.setName(moldElementDTO.getName());

            moldGroupElementList.add(moldGroupElement);
        });

        moldGroup.setListMoldGroupElement(moldGroupElementList);

        moldGroupRepository.save(moldGroup);

        /* generate progress for step in progress */
        progressMoldGroupComponent.generateProgressForCreateGroup(reqDTO, order, listUpdateMold);
    }

    private void updateMoldGroup(PurchaseOrder order, MoldGroupReqDTO reqDTO) {
        List<Mold> listUpdateMold = order.getListMold().stream()
                .filter(mold -> reqDTO.getMoldGroup().getMoldIdList().contains(mold.getId()))
                .collect(Collectors.toList());

        MoldGroupDetailReqDTO moldGroupReq = reqDTO.getMoldGroup();
        MoldGroup curMoldGroup = order.getListMoldGroup().stream()
                .filter(moldGroupInOrder ->
                        reqDTO.getMoldGroup().getId().equals(moldGroupInOrder.getId()))
                .findFirst()
                .orElseThrow(() ->
                        new BusinessException(
                                "Not found mold group id "
                                        + reqDTO.getMoldGroup().getId()
                                        + " in order "
                                        + reqDTO.getOrderId()
                        )
                );

        /* delete mold in group */
        List<Mold> listMoldRmOutGroup = curMoldGroup.getListMold().stream()
                .filter(mold -> !listUpdateMold.contains(mold))
                .collect(Collectors.toList());

        listMoldRmOutGroup.forEach(mold -> mold.setMoldGroup(null));
        progressMoldGroupComponent.deleteMoldOutOfGroup(listMoldRmOutGroup, curMoldGroup);
        curMoldGroup.setListMold(listUpdateMold);

        final boolean isChangeMoldGroupType = !moldGroupReq.getType().equals(curMoldGroup.getType());
        final boolean isChangeElement = updateMoldElement(curMoldGroup, moldGroupReq);

        processUpdateMoldGroup(curMoldGroup, moldGroupReq, listUpdateMold, order, isChangeElement, isChangeMoldGroupType);

        /* update relationship to listUpdateMold */
        listUpdateMold.forEach(updatedMold -> {
            updatedMold.setMoldGroup(curMoldGroup);
        });

        moldGroupDetailMapper.partialUpdate(curMoldGroup, moldGroupReq);

        moldGroupRepository.save(curMoldGroup);
    }

    private boolean updateMoldElement(MoldGroup curMoldGroup, MoldGroupDetailReqDTO moldGroupReq) {
        boolean isChangeElement = false;
        List<MoldGroupElement> moldGroupElementList = curMoldGroup.getListMoldGroupElement();
        List<MoldElementDTO> reqElementList = moldGroupReq.getMoldElementList();
        List<MoldElementDTO> reqElementListNotInEntity = reqElementList.stream()
                .filter(moldElementDTO ->
                        !moldGroupElementList
                                .stream()
                                .map(MoldGroupElement::getName)
                                .collect(Collectors.toList())
                                .contains(moldElementDTO.getName())
                ).collect(Collectors.toList());

        for (MoldGroupElement groupElement : moldGroupElementList) {
            Optional<MoldElementDTO> elementDTOChk = reqElementList.stream()
                    .filter(moldElementDTO
                            -> moldElementDTO.getName().equalsIgnoreCase(groupElement.getName()))
                    .findFirst();

            if (elementDTOChk.isPresent()) {
                if (elementDTOChk.get().isChecked()) {
                    /* req checked = true */
                    if (!Boolean.TRUE.equals(groupElement.getChecked())) {
                        /* current checked is not true, request is true */
                        /* update check to current element */
                        groupElement.setChecked(Boolean.TRUE);
                        isChangeElement = true;
                    }
                } else {
                    /* req checked = false */
                    if (!Boolean.FALSE.equals(groupElement.getChecked())) {
                        /* current checked is not false, request is false */
                        /* update check to current element */
                        groupElement.setChecked(Boolean.FALSE);
                        isChangeElement = true;
                    }
                }
            } else {
                /* element exist but not have in request -> delete element */
                isChangeElement = true;
                groupElement.setChecked(Boolean.FALSE);
            }
        }

        if (!CollectionUtils.isEmpty(reqElementListNotInEntity)) {
            if (!CollectionUtils.isEmpty(moldGroupElementList)) {
                isChangeElement = true;
            }

            reqElementListNotInEntity.forEach(moldElementDTO -> {
                MoldGroupElement moldGroupElement = new MoldGroupElement();
                moldGroupElement.setMoldGroup(curMoldGroup);
                moldGroupElement.setChecked(moldElementDTO.isChecked());
                moldGroupElement.setName(moldElementDTO.getName());

                moldGroupElementList.add(moldGroupElement);
            });
        }

        return isChangeElement;
    }

    private void processUpdateMoldGroup(
            MoldGroup curMoldGroup,
            MoldGroupDetailReqDTO moldGroupReq,
            List<Mold> listUpdateMold,
            PurchaseOrder order,
            final boolean isChangeElement,
            final boolean isChangeMoldGroupType) {
        for (Mold mold : listUpdateMold) {
            if (Objects.isNull(mold.getMoldGroup())) {
                /* generate progress */
                /* reset progress also can generate new progress */
                progressMoldGroupComponent.resetMoldGroupProgressChangeType(mold, moldGroupReq);
            } else if (!moldGroupReq.getId().equals(mold.getMoldGroup().getId())) {
                /* When update, mold must not in any group */
                throw new BusinessException(
                        "Mold " + mold.getSize() + " is in mold group "
                                + mold.getMoldGroup().getId() + " " + mold.getMoldGroup().getType()
                );
            } else {
                /* not change mold group */
                if (isChangeMoldGroupType) {
                    /* reset progress to step CAM_GO or 3D_GO (if type = FREEFORM) */
                    progressMoldGroupComponent.resetMoldGroupProgressChangeType(mold, moldGroupReq);
                } else if (isChangeElement) {
                    /* reset progress of element */
                    progressComponent.resetMoldGroupProgressChangeElement(mold);

                    /* check option Ban Dien */
                    if (Boolean.FALSE.equals(curMoldGroup.getHasBanDien()) && moldGroupReq.isHasBanDien()) {
                        /* Check to has Ban Dien */
                        /* Generate progress for step Phong Dien */
                        log.info("Check hasBanDien, create progress for mold {} at step PHONG DIEN", mold);
                        Step phongDienStep = StepConditionUtil.getStepPhongDien(order, stepConstant).orElseThrow();
                        log.info("Step PHONG DIEN {}", phongDienStep);

                        List<MoldProgress> moldProgressListForConditionStep = ProgressUtil.generateMoldProcessForMoldGroup(
                                phongDienStep,
                                List.of(mold));

                        phongDienStep.setListMoldProgress(moldProgressListForConditionStep);

                        log.info("Insert progress {}", moldProgressListForConditionStep);
                        moldProgressRepository.saveAll(moldProgressListForConditionStep);
                    } else if (Boolean.TRUE.equals(curMoldGroup.getHasBanDien()) && !moldGroupReq.isHasBanDien()) {
                        /* Uncheck to has Ban Dien */
                        /* Delete progress for step Phong Dien */
                        log.info("Uncheck hasBanDien, delete progress for mold {} at step PHONG DIEN", mold);
                        Step phongDienStep = StepConditionUtil.getStepPhongDien(order, stepConstant).orElseThrow();
                        log.info("Step PHONG DIEN {}", phongDienStep);

                        List<MoldProgress> listProgressNeedRemoved = phongDienStep.getListMoldProgress().stream()
                                .filter(moldProgress ->
                                        mold.getSize().equalsIgnoreCase(moldProgress.getMold().getSize()))
                                .collect(Collectors.toList());
                        phongDienStep.getListMoldProgress().removeAll(listProgressNeedRemoved);

                        log.info("Delete progress {}", listProgressNeedRemoved);
                        moldProgressRepository.deleteAll(listProgressNeedRemoved);
                    }
                }
            }
        }
    }

    private void deleteGroupHasNoMold(PurchaseOrder order) {
        if (Objects.nonNull(order.getListMoldGroup())) {
            List<MoldGroup> groupHasNoMold = order.getListMoldGroup().stream()
                    .filter(group -> CollectionUtils.isEmpty(group.getListMold()))
                    .collect(Collectors.toList());

            order.getListMoldGroup().removeAll(groupHasNoMold);

            moldGroupRepository.deleteAll(groupHasNoMold);
            log.info("Delete group {} has no mold", groupHasNoMold);
        }
    }

    @Override
    public List<MoldGroupDetailResDTO> getListElementOfOrder(long orderId) {
        PurchaseOrder order = orderRepository.findById(orderId).orElseThrow(EntityNotFoundException::new);

        return moldGroupDetailMapper.toDTOs(order.getListMoldGroup());
    }

    @Override
    public SuccessRespDTO deleteMoldGroup(Long groupId) {
        MoldGroup moldGroup = moldGroupRepository.findById(groupId).orElseThrow();

        progressMoldGroupComponent.deleteMoldGroup(moldGroup);

        moldGroup.getListMold().forEach(mold -> mold.setMoldGroup(null));
        moldGroupRepository.delete(moldGroup);

        return SuccessRespDTO.builder().message(MessageConstant.DELETE_SUCCESS).build();
    }
}
