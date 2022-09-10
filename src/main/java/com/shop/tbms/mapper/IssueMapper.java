package com.shop.tbms.mapper;

import com.shop.tbms.dto.step.report_issue.ReportIssueStepReqDTO;
import com.shop.tbms.entity.Issue;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface IssueMapper {
    Issue toEntityFromReportIssue(ReportIssueStepReqDTO reqDTO);
}
