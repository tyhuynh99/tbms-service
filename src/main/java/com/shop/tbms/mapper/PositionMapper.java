package com.shop.tbms.mapper;

import com.shop.tbms.dto.PositionDTO;
import com.shop.tbms.entity.Position;
import org.mapstruct.Mapper;

import java.util.List;

@Mapper(componentModel = "spring")
public interface PositionMapper {
    PositionDTO toDTO(Position position);

    List<PositionDTO> toListPositionDTO(List<Position> listPosition);
}
