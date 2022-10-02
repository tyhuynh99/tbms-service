package com.shop.tbms.enumerate.step;

import com.fasterxml.jackson.annotation.JsonValue;

public enum ReportType {
    BY_MOLD(0),
    BY_MOLD_ELEMENT(1),
    BY_MOLD_SEND_RECEIVE(2);

    private int value;

    ReportType(int value) {
        this.value = value;
    }

    @JsonValue
    public int getValue() {return ordinal();}
}
