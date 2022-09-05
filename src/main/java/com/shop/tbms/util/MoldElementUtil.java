package com.shop.tbms.util;

import com.shop.tbms.config.exception.BusinessException;
import com.shop.tbms.dto.step.report.ReportMoldElementReqDTO;
import com.shop.tbms.entity.MoldElement;
import com.shop.tbms.enumerate.CheckedValue;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.List;

@Slf4j
public class MoldElementUtil {
    public static void validateElementDescription(MoldElement currentElement, ReportMoldElementReqDTO reqDTO) {
        checkRequired(currentElement, reqDTO);
        checkValidValue(currentElement, reqDTO);
    }

    private static void checkRequired(MoldElement currentElement, ReportMoldElementReqDTO reqDTO) {
        if (Boolean.TRUE.equals(currentElement.getIsRequired())) {
            if (StringUtils.isBlank(reqDTO.getDescription())) {
                throw new BusinessException(String.format("Element %s is required.", currentElement.getName()));
            }
        }
    }

    private static void checkValidValue(MoldElement currentElement, ReportMoldElementReqDTO reqDTO) {
        if (StringUtils.isBlank(reqDTO.getDescription())) return;

        switch (currentElement.getType()) {
            case CHECK:
                try {
                    CheckedValue.fromValue(reqDTO.getDescription().trim());
                } catch (Exception e) {
                    log.info("Parse checked value of {}, get exception {}", reqDTO, e);
                    throw new BusinessException(
                            String.format(
                                    "Element %s get invalid description %s with type = CHECK",
                                    currentElement.getName(),
                                    reqDTO.getDescription())
                    );
                }
                break;
            case SELECT:
                List<String> listPossibleValue = Arrays.asList(currentElement.getPossibleValue().split(";"));
                if (!listPossibleValue.contains(reqDTO.getDescription())) {
                    throw new BusinessException(
                            String.format(
                                    "Element %s with type = SELECT, get invalid description. %s is not in %s.",
                                    currentElement.getName(),
                                    reqDTO.getDescription(),
                                    currentElement.getPossibleValue())
                    );
                }
                break;
            default:
        }
    }
}
