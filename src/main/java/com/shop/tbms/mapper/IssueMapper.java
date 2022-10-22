package com.shop.tbms.mapper;

import com.shop.tbms.dto.log.IssueLogDTO;
import com.shop.tbms.dto.step.report_issue.ReportIssueStepReqDTO;
import com.shop.tbms.entity.Account;
import com.shop.tbms.entity.Issue;
import com.shop.tbms.entity.IssueMoldDetail;
import com.shop.tbms.repository.AccountRepository;
import com.shop.tbms.util.MoldUtil;
import org.apache.commons.lang3.StringUtils;
import org.mapstruct.Context;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Mapper(componentModel = "spring")
public interface IssueMapper {
    Issue toEntityFromReportIssue(ReportIssueStepReqDTO reqDTO);

    @Mapping(target = "reportBy", source = "createdBy", qualifiedByName = "getReportBy")
    @Mapping(target = "reportAt", source = "updatedDate")
    @Mapping(target = "listMold", source = "issue", qualifiedByName = "getListMold")
    IssueLogDTO toLogDTO(Issue issue, @Context AccountRepository accountRepository);

    List<IssueLogDTO> toLogDTOs(List<Issue> issues, @Context AccountRepository accountRepository);

    @Named("getListMold")
    default String getListMold(Issue issue) {
        if (CollectionUtils.isEmpty(issue.getListIssueMold())) return StringUtils.EMPTY;

        return issue.getListIssueMold().stream()
                .map(IssueMoldDetail::getMold)
                .map(MoldUtil::getMoldName)
                .collect(Collectors.joining(", "));
    }

    @Named("getReportBy")
    default String getReportBy(String createdBy, @Context AccountRepository accountRepository) {
        return accountRepository.findFirstByUsername(createdBy)
                .map(Account::getFullname)
                .orElse(StringUtils.EMPTY);
    }
}
