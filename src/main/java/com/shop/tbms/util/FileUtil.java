package com.shop.tbms.util;

import static com.shop.tbms.constant.CommonConstant.PDF_FOLDER_NAME;

public class FileUtil {
    public static String generateDestination(Long orderId, String filename) {
        return PDF_FOLDER_NAME.concat(orderId + "/".concat(filename));
    }
}
