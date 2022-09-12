package com.shop.tbms.mapper.order;

import com.shop.tbms.dto.order.OrderCreateReqDTO;
import com.shop.tbms.dto.order.OrderUpdateReqDTO;
import com.shop.tbms.entity.PurchaseOrder;
import org.mapstruct.*;

@Mapper(componentModel = "spring")
public interface PurchaseOrderMapper {
    @Mapping(target = "isDeleted", source = "orderCreateReqDTO", qualifiedByName = "setDefDeleted")
    PurchaseOrder toOrderEntity(OrderCreateReqDTO orderCreateReqDTO);

    @Named("setDefDeleted")
    default Boolean setDefDeleted(OrderCreateReqDTO orderCreateReqDTO) {
        return Boolean.FALSE;
    }

    @BeanMapping(nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE)
    void partialUpdateOrderEntity(@MappingTarget PurchaseOrder originOrder, OrderUpdateReqDTO orderUpdateReqDTO);
}
