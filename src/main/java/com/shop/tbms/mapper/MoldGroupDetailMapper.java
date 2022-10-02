package com.shop.tbms.mapper;

import com.shop.tbms.dto.MoldGroupDetailDTO;
import com.shop.tbms.entity.MoldGroupDetail;
import org.mapstruct.Mapper;

import java.util.List;

@Mapper(componentModel = "spring")
public interface MoldGroupDetailMapper {
    MoldGroupDetailDTO toDTO(MoldGroupDetail moldGroupDetail);

    List<MoldGroupDetailDTO> toDTOs(List<MoldGroupDetail> moldGroupDetailList);

}
