package com.shop.tbms.enumerate.mold;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

@Getter
public enum MoldPlatingType {
    XI_MA_THUONG("Xi mạ thường"),
    XI_MA_MO("Xi mạ mờ"),
    DANH_BONG("Đánh bóng");

    MoldPlatingType(String name) {
        this.name = name;
    }

    @JsonValue
    public int getValue() {return ordinal();}

    private String name;
}
