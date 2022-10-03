package com.shop.tbms.util;

import com.shop.tbms.entity.Mold;
import com.shop.tbms.entity.MoldProgress;
import com.shop.tbms.entity.Step;

import java.util.List;
import java.util.stream.Collectors;

public class ProgressUtil {
    public static List<MoldProgress> generateMoldProcess(List<Mold> listMold, Step step) {
        return listMold.stream().map(mold -> {
            MoldProgress moldProgress = new MoldProgress();
            moldProgress.setMold(mold);
            moldProgress.setStep(step);
            moldProgress.setIsCompleted(Boolean.FALSE);

            return moldProgress;
        }).collect(Collectors.toList());
    }
}
