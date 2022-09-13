package com.shop.tbms.util;

import com.shop.tbms.entity.Evidence;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.entity.Step;

public class EvidenceUtil {
    private static final String DASH = "-";
    private static final String DOT = ".";

    public static String generateFilename(Evidence evidence) {
        Step step = evidence.getStep();
        PurchaseOrder order = step.getProcedure().getPurchaseOrder();
        return order.getCode()
                + DASH
                + step.getCode()
                + DASH
                + evidence.getId()
                + getFileExtension(evidence.getOriginFilename());
    }

    public static String getFileExtension(String filename) {
        return filename.substring(filename.lastIndexOf(DOT));
    }
}
