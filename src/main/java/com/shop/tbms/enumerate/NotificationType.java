package com.shop.tbms.enumerate;

import com.fasterxml.jackson.annotation.JsonValue;

public enum NotificationType {
    NEARLY_DUE,
    OVERDUE;

    @JsonValue
    public int getValue() {return ordinal();}
}
