package com.shop.tbms.util;

import com.shop.tbms.entity.Mold;
import com.shop.tbms.entity.MoldGroupElementProgress;

import java.util.List;
import java.util.stream.Collectors;

public class MoldElementUtil {
    public static long calPercentComplete(List<MoldGroupElementProgress> progressList, Mold mold) {
        List<MoldGroupElementProgress> progressListByMold = progressList.stream()
                .filter(elementProgress ->
                        mold.getId().equals(elementProgress.getMold().getId()))
                .collect(Collectors.toList());

        long completedElement = progressListByMold.stream()
                .filter(elementProgress -> Boolean.TRUE.equals(elementProgress.getIsCompleted()))
                .count();
        long totalElement = progressListByMold.size();

        return Math.floorDiv(completedElement, totalElement);
    }
}
