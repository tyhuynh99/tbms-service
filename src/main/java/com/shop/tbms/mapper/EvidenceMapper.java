package com.shop.tbms.mapper;

import com.shop.tbms.dto.EvidenceDTO;
import com.shop.tbms.entity.Evidence;
import org.mapstruct.Mapper;

import java.util.List;

@Mapper(componentModel = "spring")
public interface EvidenceMapper {
    EvidenceDTO toDTO(Evidence evidence);

    List<EvidenceDTO> toDTOs(List<Evidence> evidences);
}
