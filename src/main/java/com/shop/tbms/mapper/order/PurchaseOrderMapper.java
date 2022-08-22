package com.shop.tbms.mapper.order;

import com.shop.tbms.dto.order.OrderCreateReqDTO;
import com.shop.tbms.entity.PurchaseOrder;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface PurchaseOrderMapper {
    PurchaseOrder toOrderEntity(OrderCreateReqDTO orderCreateReqDTO);
}
