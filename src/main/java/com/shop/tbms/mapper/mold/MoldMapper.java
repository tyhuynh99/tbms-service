package com.shop.tbms.mapper.mold;

import com.shop.tbms.dto.mold.MoldDTO;
import com.shop.tbms.entity.Mold;
import org.mapstruct.Mapper;

import java.util.List;

@Mapper(componentModel = "spring")
public interface MoldMapper {
    MoldDTO toDTO(Mold mold);

    List<MoldDTO> toDTOs(List<Mold> molds);


}
