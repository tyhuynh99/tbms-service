package com.shop.tbms.util;

import org.apache.commons.lang3.StringUtils;
import org.springframework.format.annotation.DateTimeFormat;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class DateTimeUtil {
    public static final String PATTERN = "yyyy-MM-dd HH:mm:ss";
    public static final DateTimeFormatter FORMAT = DateTimeFormatter.ofPattern(PATTERN);

    public static String formatDate(LocalDateTime dateTime) {return FORMAT.format(dateTime);}

    public static LocalDateTime covert(String dateTimeStr) {
        return LocalDateTime.parse(dateTimeStr, FORMAT);
    }

    public static boolean isRelativeEqual(LocalDateTime dateTime1, LocalDateTime dateTime2) {
        return StringUtils.compare(formatDate(dateTime1), formatDate(dateTime2)) == 0;
    }
}
