package com.shop.tbms.util;

import com.shop.tbms.constant.ChecklistVarContent;
import com.shop.tbms.entity.Checklist;
import com.shop.tbms.entity.MoldGroup;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.enumerate.mold.MoldPlatingType;
import org.springframework.util.CollectionUtils;

import java.util.stream.Collectors;

public class ChecklistUtil {
    public static String genChecklistContent(Checklist checklist) {
        if (checklist.getContent().contains(ChecklistVarContent.PLAITING_TYPE)) {
            PurchaseOrder order = checklist.getStep().getProcedure().getPurchaseOrder();
            if (!CollectionUtils.isEmpty(order.getListMoldGroup())) {
                String plaitingType = order.getListMoldGroup().stream()
                        .map(MoldGroup::getPlatingType)
                        .map(MoldPlatingType::getName)
                        .distinct()
                        .collect(Collectors.joining(", "));

                return checklist.getContent().replace(ChecklistVarContent.PLAITING_TYPE, plaitingType);
            }
        }
        return checklist.getContent();
    }
}
