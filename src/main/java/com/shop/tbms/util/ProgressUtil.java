package com.shop.tbms.util;

import com.shop.tbms.entity.*;
import com.shop.tbms.enumerate.mold.MoldDeliverProgressType;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class ProgressUtil {

    public static List<MoldProgress> generateMoldProcess(Step step) {
        return step.getProcedure().getPurchaseOrder().getListMold().stream().map(mold -> {
            MoldProgress moldProgress = new MoldProgress();
            moldProgress.setMold(mold);
            moldProgress.setStep(step);
            moldProgress.setIsCompleted(Boolean.FALSE);

            return moldProgress;
        }).collect(Collectors.toList());
    }

    public static List<MoldDeliverProgress> generateMoldDeliverProcess(Step step) {
        List<MoldDeliverProgress> result = new ArrayList<>();

        step.getProcedure().getPurchaseOrder().getListMold().stream().forEach(mold -> {
            MoldDeliverProgress moldDeliverSendProgress = new MoldDeliverProgress();
            moldDeliverSendProgress.setMold(mold);
            moldDeliverSendProgress.setStep(step);
            moldDeliverSendProgress.setIsCompleted(Boolean.FALSE);
            moldDeliverSendProgress.setType(MoldDeliverProgressType.SEND);
            result.add(moldDeliverSendProgress);

            MoldDeliverProgress moldDeliverReceiveProgress = new MoldDeliverProgress();
            moldDeliverReceiveProgress.setMold(mold);
            moldDeliverReceiveProgress.setStep(step);
            moldDeliverReceiveProgress.setIsCompleted(Boolean.FALSE);
            moldDeliverReceiveProgress.setType(MoldDeliverProgressType.RECEIVE);
            result.add(moldDeliverReceiveProgress);
        });

        return result;
    }

    public static List<MoldGroupElementProgress> generateMoldGroupElementProgress(Step step) {
        List<MoldGroupElementProgress> result = new ArrayList<>();
        List<MoldGroup> moldGroupList = step.getProcedure().getPurchaseOrder().getListMoldGroup();

        if (!CollectionUtils.isEmpty(moldGroupList)) {
            for (MoldGroup moldGroup : moldGroupList) {
                List<Mold> moldList = moldGroup.getListMold();
                List<MoldGroupElement> moldGroupElementList = moldGroup.getListMoldGroupElement();

                for (Mold mold : moldList) {
                    for (MoldGroupElement moldGroupElement : moldGroupElementList) {
                        MoldGroupElementProgress progress = new MoldGroupElementProgress();
                        progress.setMoldGroupElement(moldGroupElement);
                        progress.setMold(mold);
                        progress.setStep(step);

                        progress.setIsCompleted(Boolean.FALSE);

                        result.add(progress);
                    }
                }
            }
        }

        return result;
    }
}
