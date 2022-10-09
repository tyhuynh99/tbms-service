package com.shop.tbms.enumerate.step;

import com.fasterxml.jackson.annotation.JsonValue;

public enum StepType {
    NORMAL(0),
    THIRD_PARTY(1),
    DELIVERY(2),
    FIXING(3);

    private int value;

    StepType(int value) {
        this.value = value;
    }

    @JsonValue
    public int getValue() {return ordinal();}
}
