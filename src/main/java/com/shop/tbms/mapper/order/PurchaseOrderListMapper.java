package com.shop.tbms.mapper.order;

import com.shop.tbms.dto.order.OrderListRespDTO;
import com.shop.tbms.entity.PurchaseOrder;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import java.util.List;

@Mapper(componentModel = "spring")
public interface PurchaseOrderListMapper {
    @Mapping(target = "numOfMold", source = "purchaseOrder", qualifiedByName = "getNumOfMold")
    @Mapping(target = "procedureName", source = "procedure.name")
    OrderListRespDTO toListResp(PurchaseOrder purchaseOrder);

    List<OrderListRespDTO> toListResp(List<PurchaseOrder> purchaseOrders);

    @Named("getNumOfMold")
    default Long getNumOfMold(PurchaseOrder purchaseOrder) {
        return Long.valueOf(purchaseOrder.getListMold().size());
    }
}
