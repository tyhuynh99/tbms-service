package com.shop.tbms.mapper.order;

import com.shop.tbms.dto.order.OrderCreateReqDTO;
import com.shop.tbms.dto.order.OrderUpdateReqDTO;
import com.shop.tbms.entity.PurchaseOrder;
import org.mapstruct.BeanMapping;
import org.mapstruct.Mapper;
import org.mapstruct.MappingTarget;
import org.mapstruct.NullValuePropertyMappingStrategy;

@Mapper(componentModel = "spring")
public interface PurchaseOrderMapper {
    PurchaseOrder toOrderEntity(OrderCreateReqDTO orderCreateReqDTO);

    @BeanMapping(nullValuePropertyMappingStrategy = NullValuePropertyMappingStrategy.IGNORE)
    void partialUpdateOrderEntity(@MappingTarget PurchaseOrder originOrder, OrderUpdateReqDTO orderUpdateReqDTO);
}
