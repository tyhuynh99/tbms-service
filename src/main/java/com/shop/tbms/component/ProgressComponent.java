package com.shop.tbms.component;

import com.shop.tbms.entity.Step;
import com.shop.tbms.repository.MoldDeliverProgressRepository;
import com.shop.tbms.repository.MoldGroupElementProgressRepository;
import com.shop.tbms.repository.MoldProgressRepository;
import com.shop.tbms.util.ProgressUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class ProgressComponent {
    @Autowired
    private MoldProgressRepository moldProgressRepository;
    @Autowired
    private MoldDeliverProgressRepository moldDeliverProgressRepository;
    @Autowired
    private MoldGroupElementProgressRepository moldGroupElementProgressRepository;

    public void generateProgressForStep(Step step) {
        log.info("Start generate progress for step {}", step);
        switch (step.getReportType()) {
            case BY_MOLD:
                moldProgressRepository.saveAll(ProgressUtil.generateMoldProcess(step));
                break;
            case BY_MOLD_SEND_RECEIVE:
                moldDeliverProgressRepository.saveAll(ProgressUtil.generateMoldDeliverProcess(step));
                break;
            case BY_MOLD_ELEMENT:
                moldGroupElementProgressRepository.saveAll(ProgressUtil.generateMoldGroupElementProgress(step));
                break;
            default:
        }
        log.info("End generate progress for step {}", step);
    }
}
