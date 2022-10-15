package com.shop.tbms.service.impl;

import com.shop.tbms.component.ProgressComponent;
import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.constant.MessageConstant;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.mold.MoldElementDTO;
import com.shop.tbms.dto.mold.MoldElementTemplateDTO;
import com.shop.tbms.dto.mold.MoldGroupDetailDTO;
import com.shop.tbms.dto.mold.MoldGroupReqDTO;
import com.shop.tbms.entity.Mold;
import com.shop.tbms.entity.MoldGroup;
import com.shop.tbms.entity.MoldGroupElement;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.enumerate.order.OrderStatus;
import com.shop.tbms.mapper.mold.MoldElementTemplateMapper;
import com.shop.tbms.mapper.mold.MoldGroupDetailMapper;
import com.shop.tbms.repository.MoldGroupRepository;
import com.shop.tbms.repository.PurchaseOrderRepository;
import com.shop.tbms.repository.TemplateMoldElementRepository;
import com.shop.tbms.service.MoldService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import javax.persistence.EntityNotFoundException;
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
    private MoldElementTemplateMapper moldElementTemplateMapper;
    @Autowired
    private MoldGroupDetailMapper moldGroupDetailMapper;

    @Autowired
    private ProgressComponent progressComponent;

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

        /* get current mold */
        List<Mold> moldList = order.getListMold();

        for (Mold mold : moldList) {
            /* get mold from request */
            MoldGroupDetailDTO detailDTO = reqDTO.getMoldGroups().stream()
                    .filter(moldGroupDetailDTO
                            -> moldGroupDetailDTO.getMoldList().contains(mold.getSize()))
                    .findFirst()
                    .orElseThrow();

            /* check current mold group */
            if (Objects.isNull(mold.getMoldGroup())) {
                mold.setMoldGroup(new MoldGroup());
            }

            MoldGroup moldGroup = mold.getMoldGroup();
            boolean isChangeMoldGroupType = !detailDTO.getType().equals(moldGroup.getType());
            boolean isChangeElement = false;

            moldGroupDetailMapper.partialUpdate(moldGroup, detailDTO);

            /* check mold group element */
            List<MoldGroupElement> moldGroupElementList = moldGroup.getListMoldGroupElement();
            List<MoldElementDTO> reqElementList = detailDTO.getMoldElementList();
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
                isChangeElement = true;

                reqElementListNotInEntity.forEach(moldElementDTO -> {
                    MoldGroupElement moldGroupElement = new MoldGroupElement();
                    moldGroupElement.setMoldGroup(moldGroup);
                    moldGroupElement.setChecked(moldElementDTO.isChecked());
                    moldGroupElement.setName(moldElementDTO.getName());

                    moldGroupElementList.add(moldGroupElement);
                });
            }

            // save entity
            moldGroupRepository.saveAndFlush(moldGroup);

            if (isChangeMoldGroupType) {
                /* reset progress to step CAM_GO */
                progressComponent.resetMoldGroupProgressChangeType(mold);
            } else if (isChangeElement) {
                /* reset progress of element */
                progressComponent.resetMoldGroupProgressChangeElement(mold);
            }
        }

        /**/
        return SuccessRespDTO.builder().message(MessageConstant.UPDATE_SUCCESS).build();
    }

    @Override
    public List<MoldGroupDetailDTO> getListElementOfOrder(long orderId) {
        PurchaseOrder order = orderRepository.findById(orderId).orElseThrow(EntityNotFoundException::new);

        return moldGroupDetailMapper.toDTOs(order.getListMoldGroup());
    }
}
