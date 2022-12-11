package com.shop.tbms.mapper;

import com.shop.tbms.dto.ChecklistDTO;
import com.shop.tbms.entity.Checklist;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ChecklistMapper {
    @Mapping(target = "checklistId", source = "id")
    @Mapping(target = "content", source = "mappedContent")
    ChecklistDTO toDTO(Checklist checklist);

    List<ChecklistDTO> toDTOs(List<Checklist> checklists);
}
