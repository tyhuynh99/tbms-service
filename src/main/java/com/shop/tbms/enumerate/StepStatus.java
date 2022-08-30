package com.shop.tbms.enumerate;

import com.fasterxml.jackson.annotation.JsonValue;

public enum StepStatus {
    INIT, IN_PROGRESS, COMPLETED;

    @JsonValue
    public int getValue() {return ordinal();}
}
