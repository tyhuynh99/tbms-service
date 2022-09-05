package com.shop.tbms.mapper;

import com.shop.tbms.dto.MoldElementDTO;
import com.shop.tbms.entity.MoldElement;
import com.shop.tbms.entity.TemplateMoldElement;
import org.mapstruct.Mapper;

import java.util.List;

@Mapper(componentModel = "spring")
public interface MoldElementMapper {
    MoldElementDTO toDTO(MoldElement moldElement);

    List<MoldElementDTO> toDTOs(List<MoldElement> moldElements);

    MoldElement fromTemplate(TemplateMoldElement templateMoldElement);

    List<MoldElement> fromTemplates(List<TemplateMoldElement> templateMoldElementList);
}
