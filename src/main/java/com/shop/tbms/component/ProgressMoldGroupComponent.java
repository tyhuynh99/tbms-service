package com.shop.tbms.component;

import com.shop.tbms.constant.StepConstant;
import com.shop.tbms.dto.mold.MoldGroupReqDTO;
import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.mold.MoldType;
import com.shop.tbms.enumerate.step.StepType;
import com.shop.tbms.repository.MoldDeliverProgressRepository;
import com.shop.tbms.repository.MoldGroupElementProgressRepository;
import com.shop.tbms.repository.MoldProgressRepository;
import com.shop.tbms.util.ProgressUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

@Slf4j
public class ProgressMoldGroupComponent {
    @Autowired
    private StepConstant stepConstant;

    @Autowired
    private MoldProgressRepository moldProgressRepository;
    @Autowired
    private MoldGroupElementProgressRepository moldGroupElementProgressRepository;
    @Autowired
    private MoldDeliverProgressRepository moldDeliverProgressRepository;

    public void generateProgressForCreateGroup(MoldGroupReqDTO reqDTO, PurchaseOrder order, List<Mold> listUpdateMold) {
        List<MoldProgress> listUpdatedMoldProgress = new ArrayList<>();
        List<MoldDeliverProgress> listUpdatedMoldDeliverProgress = new ArrayList<>();
        List<MoldGroupElementProgress> listUpdatedMoldElementProgress = new ArrayList<>();

        boolean isFreeFormType = MoldType.FREEFORM.equals(reqDTO.getMoldGroup().getType());

        for (Step step : order.getProcedure().getListStep()) {
            if (stepConstant.getCodePHONG_DIEN().equalsIgnoreCase(step.getCode())) {
                /* gen step Phong dien */
                if (reqDTO.getMoldGroup().isHasBanDien() || isFreeFormType) {
                    List<MoldProgress> moldProgressListForConditionStep = ProgressUtil.generateMoldProcessForMoldGroup(
                            step,
                            listUpdateMold);

                    step.setListMoldProgress(moldProgressListForConditionStep);

                    listUpdatedMoldProgress.addAll(moldProgressListForConditionStep);
                }
            }

            if ((!isFreeFormType || (!stepConstant.getListStepNotForFreeFormType().contains(step.getCode()))) && !StepType.FIXING.equals(step.getType())) {
                switch (step.getReportType()) {
                    case BY_MOLD:
                        List<MoldProgress> moldProgressListForMoldGroup = ProgressUtil.generateMoldProcessForMoldGroup(
                                step,
                                listUpdateMold);

                        step.setListMoldProgress(moldProgressListForMoldGroup);

                        listUpdatedMoldProgress.addAll(moldProgressListForMoldGroup);
                        break;
                    case BY_MOLD_SEND_RECEIVE:
                        List<MoldDeliverProgress> moldDeliverProgressListForMoldGroup = ProgressUtil.generateMoldDeliverProcessForMoldGroup(
                                step,
                                listUpdateMold);

                        step.setListMoldDeliverProgress(moldDeliverProgressListForMoldGroup);

                        listUpdatedMoldDeliverProgress.addAll(moldDeliverProgressListForMoldGroup);
                        break;
                    case BY_MOLD_ELEMENT:
                        List<MoldGroupElementProgress> moldGroupElementProgressList = ProgressUtil.generateMoldGroupElementProgress(
                                step,
                                listUpdateMold);

                        step.setListMoldGroupElementProgresses(moldGroupElementProgressList);

                        listUpdatedMoldElementProgress.addAll(moldGroupElementProgressList);
                        break;
                    default:
                }
            }
        }

        moldProgressRepository.saveAll(listUpdatedMoldProgress);
        moldDeliverProgressRepository.saveAll(listUpdatedMoldDeliverProgress);
        moldGroupElementProgressRepository.saveAll(listUpdatedMoldElementProgress);
    }
}
