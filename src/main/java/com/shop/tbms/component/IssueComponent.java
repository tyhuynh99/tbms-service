package com.shop.tbms.component;

import com.shop.tbms.dto.step.report_issue.ReportIssueStepReqDTO;
import com.shop.tbms.entity.Issue;
import com.shop.tbms.entity.IssueMoldDetail;
import com.shop.tbms.entity.Mold;
import com.shop.tbms.entity.Step;
import com.shop.tbms.mapper.IssueMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class IssueComponent {
    @Autowired
    private IssueMapper issueMapper;

    public Issue generateEntity(ReportIssueStepReqDTO reqDTO, List<Mold> listAllMold, Step currentStep) {
        Issue issue = issueMapper.toEntityFromReportIssue(reqDTO);
        issue.setStep(currentStep);

        List<IssueMoldDetail> listMoldIssueDetail = new ArrayList<>();
        listAllMold.stream()
                .filter(mold -> reqDTO.getListMoldId().contains(mold.getId()))
                .forEach(mold -> {
                    IssueMoldDetail issueMoldDetail = new IssueMoldDetail();
                    issueMoldDetail.setIssue(issue);
                    issueMoldDetail.setMold(mold);

                    listMoldIssueDetail.add(issueMoldDetail);
                });

        issue.setListIssueMold(listMoldIssueDetail);

        return issue;
    }
}
