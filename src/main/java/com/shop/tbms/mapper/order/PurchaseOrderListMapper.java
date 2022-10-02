package com.shop.tbms.mapper.order;

import com.shop.tbms.dto.order.OrderListRespDTO;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.enumerate.order.OrderDisplayStatus;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import java.util.List;
import java.util.Objects;

@Mapper(componentModel = "spring")
public interface PurchaseOrderListMapper {
    @Mapping(target = "numOfMold", source = "purchaseOrder", qualifiedByName = "getNumOfMold")
    @Mapping(target = "procedureName", source = "procedure.name")
    @Mapping(target = "urgent", source = "isUrgent", qualifiedByName = "mapBoolean")
    @Mapping(target = "status", source = "purchaseOrder", qualifiedByName = "genDisplayStatus")
    OrderListRespDTO toListResp(PurchaseOrder purchaseOrder);

    List<OrderListRespDTO> toListResp(List<PurchaseOrder> purchaseOrders);

    @Named("getNumOfMold")
    default Long getNumOfMold(PurchaseOrder purchaseOrder) {
        return Long.valueOf(purchaseOrder.getListMold().size());
    }

    @Named("mapBoolean")
    default boolean mapBoolean(Boolean booleanVal) {
        return Objects.nonNull(booleanVal) ? booleanVal.booleanValue() : false;
    }

    @Named("genDisplayStatus")
    default OrderDisplayStatus genDisplayStatus(PurchaseOrder purchaseOrder) {
        return OrderDisplayStatus.generate(purchaseOrder);
    }
}
