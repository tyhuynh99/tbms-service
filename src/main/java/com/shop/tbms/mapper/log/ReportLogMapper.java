package com.shop.tbms.mapper.log;

import com.shop.tbms.dto.log.ReportLogDetailDTO;
import com.shop.tbms.entity.Account;
import com.shop.tbms.entity.ReportLog;
import com.shop.tbms.mapper.EvidenceMapper;
import com.shop.tbms.repository.AccountRepository;
import org.apache.commons.lang3.StringUtils;
import org.mapstruct.Context;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;

import java.util.List;

@Mapper(componentModel = "spring", uses = {EvidenceMapper.class})
public interface ReportLogMapper {
    @Mapping(target = "reportAt", source = "updatedDate")
    @Mapping(target = "reportBy", source = "createdBy", qualifiedByName = "getReportBy")
    ReportLogDetailDTO toDTO(ReportLog log, @Context AccountRepository accountRepository);

    List<ReportLogDetailDTO> toDTOs(List<ReportLog> logs, @Context AccountRepository accountRepository);

    @Named("getReportBy")
    default String getReportBy(String createdBy, @Context AccountRepository accountRepository) {
        return accountRepository.findFirstByUsername(createdBy)
                .map(Account::getFullname)
                .orElse(StringUtils.EMPTY);
    }
}
