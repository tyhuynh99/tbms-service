package com.shop.tbms.mapper;

import com.shop.tbms.constant.ChecklistVarContent;
import com.shop.tbms.dto.ChecklistDTO;
import com.shop.tbms.entity.Checklist;
import com.shop.tbms.entity.MoldGroup;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.enumerate.mold.MoldPlatingType;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.stream.Collectors;

@Mapper(componentModel = "spring")
public interface ChecklistMapper {
    @Mapping(target = "checklistId", source = "id")
    @Mapping(target = "content", source = "checklist", qualifiedByName = "genChecklistContent")
    ChecklistDTO toDTO(Checklist checklist);

    List<ChecklistDTO> toDTOs(List<Checklist> checklists);

    @Named("genChecklistContent")
    default String genChecklistContent(Checklist checklist) {
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
