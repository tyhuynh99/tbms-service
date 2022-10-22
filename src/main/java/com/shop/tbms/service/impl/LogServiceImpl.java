package com.shop.tbms.service.impl;

import com.shop.tbms.component.ProgressComponent;
import com.shop.tbms.dto.log.IssueLogDTO;
import com.shop.tbms.dto.log.ReportLogDetailDTO;
import com.shop.tbms.dto.log.ReportLogRespDTO;
import com.shop.tbms.dto.log.ReportProgressCompleteDTO;
import com.shop.tbms.entity.Issue;
import com.shop.tbms.entity.ReportLog;
import com.shop.tbms.entity.Step;
import com.shop.tbms.entity.common.AbstractAuditingEntity;
import com.shop.tbms.mapper.IssueMapper;
import com.shop.tbms.mapper.log.ReportLogMapper;
import com.shop.tbms.repository.AccountRepository;
import com.shop.tbms.repository.IssueRepository;
import com.shop.tbms.repository.ReportLogRepository;
import com.shop.tbms.service.LogService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

@Service
@Transactional(readOnly = true)
@Slf4j
public class LogServiceImpl implements LogService {
    @Autowired
    private ReportLogRepository reportLogRepository;
    @Autowired
    private IssueRepository issueRepository;
    @Autowired
    private AccountRepository accountRepository;

    @Autowired
    private ReportLogMapper reportLogMapper;
    @Autowired
    private IssueMapper issueMapper;

    @Autowired
    private ProgressComponent progressComponent;

    @Override
    public ReportLogRespDTO getReportLog(long stepId) {
        List<ReportLog> reportLogList = reportLogRepository.findByStepId(stepId);

        if (!CollectionUtils.isEmpty(reportLogList)) {
            Step step = reportLogList.stream().map(ReportLog::getStep).findAny().orElseThrow();

            List<ReportLogDetailDTO> detailDTOS = reportLogMapper.toDTOs(
                    reportLogList.stream()
                            .sorted(Comparator.comparing(AbstractAuditingEntity::getCreatedDate))
                            .collect(Collectors.toList())
            );
            List<ReportProgressCompleteDTO> completeDTOS = progressComponent.getListCompleteReport(step)
                    .stream()
                    .sorted(Comparator.comparing(ReportProgressCompleteDTO::getMold))
                    .collect(Collectors.toList());

            return ReportLogRespDTO.builder()
                    .completeTime(completeDTOS)
                    .log(detailDTOS)
                    .build();
        }

        return ReportLogRespDTO.builder().build();
    }

    @Override
    public List<IssueLogDTO> getIssueLog(long stepId) {
        List<Issue> issues = issueRepository.findByStepId(stepId);

        if (!CollectionUtils.isEmpty(issues)) {
            return issueMapper.toLogDTOs(issues, accountRepository);
        }
        return List.of();
    }
}
