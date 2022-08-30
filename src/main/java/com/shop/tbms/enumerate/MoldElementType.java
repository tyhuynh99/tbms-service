package com.shop.tbms.enumerate;

import com.fasterxml.jackson.annotation.JsonValue;

public enum MoldElementType {
    CHECK, INPUT, SELECT;

    @JsonValue
    public int getValue() {return ordinal();}
}
