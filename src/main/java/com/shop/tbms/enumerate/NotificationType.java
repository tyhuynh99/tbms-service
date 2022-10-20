package com.shop.tbms.enumerate;

import com.fasterxml.jackson.annotation.JsonValue;

public enum NotificationType {
    OTHER,
    ORDER_NEARLY_DUE,
    ORDER_OVERDUE,
    STEP_NEARLY_LATE,
    STEP_LATE;

    @JsonValue
    public int getValue() {return ordinal();}
}
