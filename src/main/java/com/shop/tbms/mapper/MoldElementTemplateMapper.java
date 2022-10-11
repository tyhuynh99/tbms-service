package com.shop.tbms.mapper;

import com.shop.tbms.dto.mold.MoldElementTemplateDTO;
import com.shop.tbms.entity.TemplateMoldElement;
import org.mapstruct.Mapper;

import java.util.List;

@Mapper(componentModel = "spring")
public interface MoldElementTemplateMapper {
    MoldElementTemplateDTO toDTO(TemplateMoldElement entity);

    List<MoldElementTemplateDTO> toDTOs(List<TemplateMoldElement> listEntities);
}
