package com.shop.tbms.mapper.log;

import com.shop.tbms.dto.log.ReportLogDetailDTO;
import com.shop.tbms.entity.ReportLog;
import com.shop.tbms.mapper.EvidenceMapper;
import org.mapstruct.Mapper;

import java.util.List;

@Mapper(componentModel = "spring", uses = {EvidenceMapper.class})
public interface ReportLogMapper {
    ReportLogDetailDTO toDTO(ReportLog log);

    List<ReportLogDetailDTO> toDTOs(List<ReportLog> logs);
}
