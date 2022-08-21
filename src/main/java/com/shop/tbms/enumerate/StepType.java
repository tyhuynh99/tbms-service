package com.shop.tbms.enumerate;

import com.fasterxml.jackson.annotation.JsonValue;

public enum StepType {
    NORMAL, THIRD_PARTY, DELIVERY;

    @JsonValue
    public int getValue() {return ordinal();}
}
