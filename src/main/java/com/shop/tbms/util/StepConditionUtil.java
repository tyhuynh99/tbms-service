package com.shop.tbms.util;

import com.shop.tbms.constant.StepConstant;
import com.shop.tbms.dto.mold.MoldGroupReqDTO;
import com.shop.tbms.entity.Mold;
import com.shop.tbms.entity.MoldProgress;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.entity.Step;
import com.shop.tbms.enumerate.mold.MoldType;
import com.shop.tbms.repository.MoldProgressRepository;
import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Slf4j
public class StepConditionUtil {
    public static boolean isStepHasConditionProgress(Step step, StepConstant stepConstant) {
        if (Boolean.TRUE.equals(step.getHasCondition())) {
            return true;
        }
        return false;
    }

    public static Optional<Step> getStepPhongDien(PurchaseOrder order, StepConstant stepConstant) {
        return order.getProcedure().getListStep().stream()
                .filter(step -> stepConstant.getCodePHONG_DIEN().equalsIgnoreCase(step.getCode()))
                .findFirst();
    }

    public static List<Step> getStepForMoldNotFreeFormType(PurchaseOrder order, StepConstant stepConstant) {
        List<String> listStepNotForFreeFormType = stepConstant.getListStepNotForFreeFormType();
        return order.getProcedure().getListStep().stream()
                .filter(step -> listStepNotForFreeFormType.contains(step.getCode()))
                .collect(Collectors.toList());
    }

    public static void generateProgressForConditionStepAfterCreateMoldGroup(
            MoldGroupReqDTO reqDTO,
            List<Mold> listUpdateMold,
            PurchaseOrder order,
            StepConstant stepConstant,
            MoldProgressRepository moldProgressRepository)
    {
        if (reqDTO.getMoldGroup().isHasBanDien() || MoldType.FREEFORM.equals(reqDTO.getMoldGroup().getType())) {
            /* Check to has Ban Dien or type = Free Form */
            /* Generate progress for step Phong Dien */
            log.info("Check hasBanDien, create progress for mold {} at step PHONG DIEN", listUpdateMold);
            Step phongDienStep = StepConditionUtil.getStepPhongDien(order, stepConstant).orElseThrow();
            log.info("Step PHONG DIEN {}", phongDienStep);

            List<MoldProgress> moldProgressListForConditionStep = ProgressUtil.generateMoldProcessForMoldGroup(
                    phongDienStep,
                    listUpdateMold);

            phongDienStep.setListMoldProgress(moldProgressListForConditionStep);

            log.info("Insert progress {}", moldProgressListForConditionStep);
            moldProgressRepository.saveAll(moldProgressListForConditionStep);
        }

        if (!MoldType.FREEFORM.equals(reqDTO.getMoldGroup().getType())) {
            /* Generate progress for mold type not FREEFORM */
            List<Step> listStepNeedGen = getStepForMoldNotFreeFormType(order, stepConstant);

            for (Step stepNeedGen : listStepNeedGen) {
                switch (stepNeedGen.getReportType()) {
                    case BY_MOLD:
                        List<MoldProgress> moldProgressListForConditionStep = ProgressUtil.generateMoldProcessForMoldGroup(
                                stepNeedGen,
                                listUpdateMold);

                        stepNeedGen.setListMoldProgress(moldProgressListForConditionStep);

                        log.info("Insert progress {}", moldProgressListForConditionStep);
                        moldProgressRepository.saveAll(moldProgressListForConditionStep);
                        break;
                    case BY_MOLD_SEND_RECEIVE:

                        break;
                    default:
                }
            }
        }
    }
}
