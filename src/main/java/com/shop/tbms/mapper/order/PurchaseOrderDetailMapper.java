package com.shop.tbms.mapper.order;

import com.shop.tbms.dto.mold.MoldDTO;
import com.shop.tbms.dto.mold.MoldGroupDetailDTO;
import com.shop.tbms.dto.order.OrderDetailRespDTO;
import com.shop.tbms.dto.order.OrderStepRespDTO;
import com.shop.tbms.entity.Mold;
import com.shop.tbms.entity.MoldGroup;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.entity.Step;
import com.shop.tbms.enumerate.order.OrderDisplayStatus;
import com.shop.tbms.mapper.mold.MoldMapper;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Mapper(componentModel = "spring", uses = {MoldMapper.class})
public interface PurchaseOrderDetailMapper {
    /* PURCHASE ORDER */
    @Mapping(target = "listMoldGroupDetail", source = "order", qualifiedByName = "genListMoldGroupDetail")
    @Mapping(target = "listStep", source = "order", qualifiedByName = "genListStep")
    @Mapping(target = "status", source = "order", qualifiedByName = "genDisplayStatus")
    @Mapping(target = "procedureName", source = "procedure.name")
    OrderDetailRespDTO fromEntityToDetailDTO(PurchaseOrder order);

    @Named("genDisplayStatus")
    default OrderDisplayStatus genDisplayStatus(PurchaseOrder order) {
        return OrderDisplayStatus.generate(order);
    }

    /* MOLD ELEMENT */
    MoldGroupDetailDTO toMoldGroupDetailDTO(MoldGroup moldGroup);
    List<MoldGroupDetailDTO> toMoldGroupDetailDTO(List<MoldGroup> moldGroupList);

    /* STEP */
    OrderStepRespDTO toStepDTO(Step step);
    List<OrderStepRespDTO> toStepDTO(List<Step> step);

    @Named("genListMoldGroupDetail")
    default List<MoldGroupDetailDTO> genListMoldGroupDetail(PurchaseOrder order) {
        return toMoldGroupDetailDTO(order.getListMoldGroup());
    }

    @Named("genListStep")
    default List<OrderStepRespDTO> genListStep(PurchaseOrder order) {
        return toStepDTO(order.getProcedure().getListStep());
    }
}
