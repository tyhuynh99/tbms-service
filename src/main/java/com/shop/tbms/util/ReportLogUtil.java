package com.shop.tbms.util;

import com.shop.tbms.constant.LogConstant;
import com.shop.tbms.dto.step.report.*;
import com.shop.tbms.dto.step.report_issue.ReportIssueStepReqDTO;
import com.shop.tbms.entity.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.stream.Collectors;

public class ReportLogUtil {
    private static final String DELIMETER = ", ";

    public static List<String> genLogForCheckList(
            List<Checklist> listChangeToChecked,
            List<Checklist> listChangeToUnchecked,
            LogConstant logConstant) {
        List<String> result = new ArrayList<>();

        if (!CollectionUtils.isEmpty(listChangeToChecked)) {
            result.add(String.format(
                    logConstant.getChecklistChecked(),
                    listChangeToChecked.stream()
                            .map(Checklist::getContent)
                            .collect(Collectors.joining(DELIMETER)))
            );
        }

        if (!CollectionUtils.isEmpty(listChangeToUnchecked)) {
            result.add(String.format(
                    logConstant.getChecklistUnchecked(),
                    listChangeToUnchecked.stream()
                            .map(Checklist::getContent)
                            .collect(Collectors.joining(DELIMETER)))
            );
        }

        return result;
    }

    public static List<String> genLogForMoldProgress(
            List<MoldProgress> listChangeToComplete,
            List<MoldProgress> listChangeToUncomplete,
            LogConstant logConstant) {
        List<String> result = new ArrayList<>();

        if (!CollectionUtils.isEmpty(listChangeToComplete)) {
            result.add(String.format(
                    logConstant.getProgressChecked(),
                    listChangeToComplete.stream()
                            .map(moldProgress ->
                                    moldProgress.getMold().getSize()
                                            + Optional.ofNullable(moldProgress.getMold().getMoldGroup())
                                            .map(MoldGroup::getType)
                                            .map(moldType -> "#" + moldType.name())
                                            .orElse(StringUtils.EMPTY)
                            ).collect(Collectors.joining(DELIMETER)))
            );
        }

        if (!CollectionUtils.isEmpty(listChangeToUncomplete)) {
            result.add(String.format(
                    logConstant.getProgressUnchecked(),
                    listChangeToUncomplete.stream()
                            .map(moldProgress ->
                                    moldProgress.getMold().getSize()
                                            + Optional.ofNullable(moldProgress.getMold().getMoldGroup())
                                            .map(MoldGroup::getType)
                                            .map(moldType -> "#" + moldType.name())
                                            .orElse(StringUtils.EMPTY)
                            ).collect(Collectors.joining(DELIMETER)))
            );
        }

        return result;
    }

    public static List<String> genLogForMoldElementProgress(
            List<MoldGroupElementProgress> listChangeToComplete,
            List<MoldGroupElementProgress> listChangeToUncomplete,
            LogConstant logConstant) {
        List<String> result = new ArrayList<>();

        Map<Long, List<MoldGroupElementProgress>> mapChangeToComplete = new HashMap<>();
        Map<Long, List<MoldGroupElementProgress>> mapChangeToUnComplete = new HashMap<>();

        listChangeToComplete.forEach(moldGroupElementProgress -> {
            Long moldId = moldGroupElementProgress.getMold().getId();
            if (mapChangeToComplete.containsKey(moldId)) {
                mapChangeToComplete.get(moldId).add(moldGroupElementProgress);
            } else {
                List<MoldGroupElementProgress> progressList = Arrays.asList(moldGroupElementProgress);
                mapChangeToComplete.put(moldId, progressList);
            }
        });

        listChangeToUncomplete.forEach(moldGroupElementProgress -> {
            Long moldId = moldGroupElementProgress.getMold().getId();
            if (mapChangeToUnComplete.containsKey(moldId)) {
                mapChangeToUnComplete.get(moldId).add(moldGroupElementProgress);
            } else {
                List<MoldGroupElementProgress> progressList = Arrays.asList(moldGroupElementProgress);
                mapChangeToUnComplete.put(moldId, progressList);
            }
        });

        if (!CollectionUtils.isEmpty(listChangeToComplete)) {
            result.add(String.format(
                    logConstant.getProgressChecked(),
                    mapChangeToComplete.keySet().stream().map(moldId -> {
                        List<MoldGroupElementProgress> progressList = mapChangeToComplete.get(moldId);

                        Mold mold = progressList.stream().findAny().orElseThrow().getMold();
                        return "{ "
                                + mold.getSize()
                                + "#"
                                + mold.getMoldGroup().getType().name()
                                + ": "
                                + progressList.stream()
                                    .map(elementProgress -> elementProgress.getMoldGroupElement().getName())
                                    .collect(Collectors.joining(DELIMETER))
                                + " }";
                    }).collect(Collectors.joining(DELIMETER)))
            );
        }

        if (!CollectionUtils.isEmpty(listChangeToUncomplete)) {
            result.add(String.format(
                    logConstant.getProgressUnchecked(),
                    mapChangeToUnComplete.keySet().stream().map(moldId -> {
                        List<MoldGroupElementProgress> progressList = mapChangeToUnComplete.get(moldId);

                        Mold mold = progressList.stream().findAny().orElseThrow().getMold();
                        return "{ "
                                + mold.getSize()
                                + "#"
                                + mold.getMoldGroup().getType().name()
                                + ": "
                                + progressList.stream()
                                .map(elementProgress -> elementProgress.getMoldGroupElement().getName())
                                .collect(Collectors.joining(DELIMETER))
                                + " }";
                    }).collect(Collectors.joining(DELIMETER)))
            );
        }

        return result;
    }

    public static List<String> genLogForMoldDeliverProgress(
            List<MoldDeliverProgress> listChangeToComplete,
            List<MoldDeliverProgress> listChangeToUncomplete,
            LogConstant logConstant) {
        List<String> result = new ArrayList<>();

        Map<Long, List<MoldDeliverProgress>> mapChangeToComplete = new HashMap<>();
        Map<Long, List<MoldDeliverProgress>> mapChangeToUnComplete = new HashMap<>();

        listChangeToComplete.forEach(moldDeliverProgresses -> {
            Long moldId = moldDeliverProgresses.getMold().getId();
            if (mapChangeToComplete.containsKey(moldId)) {
                mapChangeToComplete.get(moldId).add(moldDeliverProgresses);
            } else {
                List<MoldDeliverProgress> progressList = Arrays.asList(moldDeliverProgresses);
                mapChangeToComplete.put(moldId, progressList);
            }
        });

        listChangeToUncomplete.forEach(moldDeliverProgresses -> {
            Long moldId = moldDeliverProgresses.getMold().getId();
            if (mapChangeToUnComplete.containsKey(moldId)) {
                mapChangeToUnComplete.get(moldId).add(moldDeliverProgresses);
            } else {
                List<MoldDeliverProgress> progressList = Arrays.asList(moldDeliverProgresses);
                mapChangeToUnComplete.put(moldId, progressList);
            }
        });

        if (!CollectionUtils.isEmpty(listChangeToComplete)) {
            result.add(String.format(
                    logConstant.getProgressChecked(),
                    mapChangeToComplete.keySet().stream().map(moldId -> {
                        List<MoldDeliverProgress> progressList = mapChangeToComplete.get(moldId);

                        Mold mold = progressList.stream().findAny().orElseThrow().getMold();
                        return "{ "
                                + mold.getSize()
                                + "#"
                                + mold.getMoldGroup().getType().name()
                                + ": "
                                + progressList.stream()
                                .map(elementProgress -> elementProgress.getType().getDisplayName())
                                .collect(Collectors.joining(DELIMETER))
                                + " }";
                    }).collect(Collectors.joining(DELIMETER)))
            );
        }

        if (!CollectionUtils.isEmpty(listChangeToUncomplete)) {
            result.add(String.format(
                    logConstant.getProgressUnchecked(),
                    mapChangeToUnComplete.keySet().stream().map(moldId -> {
                        List<MoldDeliverProgress> progressList = mapChangeToUnComplete.get(moldId);

                        Mold mold = progressList.stream().findAny().orElseThrow().getMold();
                        return "{ "
                                + mold.getSize()
                                + "#"
                                + mold.getMoldGroup().getType().name()
                                + ": "
                                + progressList.stream()
                                .map(elementProgress -> elementProgress.getType().getDisplayName())
                                .collect(Collectors.joining(DELIMETER))
                                + " }";
                    }).collect(Collectors.joining(DELIMETER)))
            );
        }

        return result;
    }
}
