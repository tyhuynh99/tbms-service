package com.shop.tbms.mapper;

import com.shop.tbms.dto.ChecklistDTO;
import com.shop.tbms.entity.Checklist;
import org.mapstruct.Mapper;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ChecklistMapper {
    ChecklistDTO toDTO(Checklist checklist);

    List<ChecklistDTO> toDTOs(List<Checklist> checklists);
}
