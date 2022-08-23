package com.shop.tbms.mapper.order;

import com.shop.tbms.dto.MoldDTO;
import com.shop.tbms.dto.MoldElementDTO;
import com.shop.tbms.dto.order.OrderDetailRespDTO;
import com.shop.tbms.dto.order.OrderStepRespDTO;
import com.shop.tbms.entity.Mold;
import com.shop.tbms.entity.MoldElement;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.entity.Step;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import java.util.List;

@Mapper(componentModel = "spring")
public interface PurchaseOrderDetailMapper {
    /* PURCHASE ORDER */
    @Mapping(target = "listMold", source = "order", qualifiedByName = "genListMold")
    @Mapping(target = "listMoldElement", source = "order", qualifiedByName = "genListMoldElement")
    @Mapping(target = "listStep", source = "order", qualifiedByName = "genListStep")
    @Mapping(target = "procedureName", source = "procedure.name")
    OrderDetailRespDTO fromEntityToDetailDTO(PurchaseOrder order);

    /* MOLD */
    MoldDTO toMoldDTO(Mold mold);
    List<MoldDTO> toMoldDTO(List<Mold> mold);

    /* MOLD ELEMENT */
    MoldElementDTO toMoldElementDTO(MoldElement moldElement);
    List<MoldElementDTO> toMoldElementDTO(List<MoldElement> moldElement);

    /* STEP */
    OrderStepRespDTO toStepDTO(Step step);
    List<OrderStepRespDTO> toStepDTO(List<Step> step);

    @Named("genListMold")
    default List<MoldDTO> genListMold(PurchaseOrder order) {
        return toMoldDTO(order.getListMold());
    }

    @Named("genListMoldElement")
    default List<MoldElementDTO> genListMoldElement(PurchaseOrder order) {
        return toMoldElementDTO(order.getListMoldElement());
    }

    @Named("genListStep")
    default List<OrderStepRespDTO> genListStep(PurchaseOrder order) {
        return toStepDTO(order.getProcedure().getListStep());
    }
}
