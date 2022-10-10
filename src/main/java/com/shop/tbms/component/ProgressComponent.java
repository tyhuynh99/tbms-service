package com.shop.tbms.component;

import com.shop.tbms.dto.step.detail.progress.MoldDeliverProgressDTO;
import com.shop.tbms.dto.step.detail.progress.MoldElementProgressDTO;
import com.shop.tbms.dto.step.detail.progress.MoldElementProgressDetailDTO;
import com.shop.tbms.dto.step.detail.progress.MoldProgressDTO;
import com.shop.tbms.entity.MoldDeliverProgress;
import com.shop.tbms.entity.MoldGroupElementProgress;
import com.shop.tbms.entity.MoldProgress;
import com.shop.tbms.entity.Step;
import com.shop.tbms.enumerate.step.ReportType;
import com.shop.tbms.repository.MoldDeliverProgressRepository;
import com.shop.tbms.repository.MoldGroupElementProgressRepository;
import com.shop.tbms.repository.MoldProgressRepository;
import com.shop.tbms.util.ProgressUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

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

    public List<MoldProgressDTO> setCanCheckForMoldProgress(Step preStep, List<MoldProgressDTO> moldProgressDTOList) {
        return moldProgressDTOList.stream().map(moldProgressDTO -> {
            boolean canCheck = canCheckCompleteBySize(preStep, moldProgressDTO.getMoldSize());
            log.info("Set value canCheck of {} is {}", moldProgressDTO, canCheck);
            moldProgressDTO.setCanCheck(canCheck);

            return moldProgressDTO;
        }).collect(Collectors.toList());
    }

    public List<MoldElementProgressDTO> setCanCheckForMoldElementProgress(Step preStep, List<MoldElementProgressDTO> moldElementProgressDTOList) {
        return moldElementProgressDTOList.stream().map(moldElementProgressDTO -> {
            boolean canCheck = canCheckCompleteBySize(preStep, moldElementProgressDTO.getMoldSize());
            log.info("Set value canCheck of {} is {}", moldElementProgressDTO, canCheck);
            List<MoldElementProgressDetailDTO> moldElementProgressDetailDTOList = moldElementProgressDTO.getListElement().stream().map(moldElementProgressDetailDTO -> {
                moldElementProgressDetailDTO.setCanCheck(canCheck);
                return moldElementProgressDetailDTO;
            }).collect(Collectors.toList());

            moldElementProgressDTO.setListElement(moldElementProgressDetailDTOList);

            return moldElementProgressDTO;
        }).collect(Collectors.toList());
    }

    public List<MoldDeliverProgressDTO> setCanCheckForDeliveryProgress(Step preStep, List<MoldDeliverProgressDTO> moldDeliverProgressDTOList) {
        return moldDeliverProgressDTOList.stream().map(moldDeliverProgressDTO -> {
            boolean canCheck = canCheckCompleteBySize(preStep, moldDeliverProgressDTO.getMoldSize());
            log.info("Set value canCheck of {} is {}", moldDeliverProgressDTO, canCheck);
            moldDeliverProgressDTO.setCanCheck(canCheck);

            return moldDeliverProgressDTO;
        }).collect(Collectors.toList());
    }

    public boolean canCheckCompleteBySize(Step preStep, String moldSize) {
        switch (preStep.getReportType()) {
            case BY_MOLD:
                log.info("Check complete of mold size {} with preStep mold progress {}", moldSize, preStep.getListMoldProgress());
                return preStep.getListMoldProgress().stream()
                        .anyMatch(moldProgress ->
                                moldSize.equals(moldProgress.getMold().getSize())
                                    && Boolean.TRUE.equals(moldProgress.getIsCompleted())
                        );
            case BY_MOLD_ELEMENT:
                log.info("Check complete of mold size {} with preStep mold element progress {}", moldSize, preStep.getListMoldGroupElementProgresses());
                return preStep.getListMoldGroupElementProgresses().stream()
                        .filter(moldGroupElementProgress ->
                                moldSize.equals(moldGroupElementProgress.getMold().getSize()))
                        .allMatch(MoldGroupElementProgress::getIsCompleted);
            case BY_MOLD_SEND_RECEIVE:
                log.info("Check complete of mold size {} with preStep mold deliver progress {}", moldSize, preStep.getListMoldDeliverProgress());
                return preStep.getListMoldDeliverProgress().stream()
                        .filter(moldDeliverProgress ->
                                moldSize.equals(moldDeliverProgress.getMold().getSize()))
                        .allMatch(MoldDeliverProgress::getIsCompleted);
            default:
                return false;
        }
    }
}
