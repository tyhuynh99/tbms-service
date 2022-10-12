package com.shop.tbms.enumerate.mold;

import com.fasterxml.jackson.annotation.JsonValue;

public enum MoldDeliverProgressType {
    SEND,
    RECEIVE;

    @JsonValue
    public int getValue() {return ordinal();}
}
