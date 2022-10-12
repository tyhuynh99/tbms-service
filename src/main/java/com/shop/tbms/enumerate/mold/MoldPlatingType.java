package com.shop.tbms.enumerate.mold;

import com.fasterxml.jackson.annotation.JsonValue;

public enum MoldPlatingType {
    XI_MA_THUONG,
    XI_MA_MO,
    DANH_BONG;

    @JsonValue
    public int getValue() {return ordinal();}
}
