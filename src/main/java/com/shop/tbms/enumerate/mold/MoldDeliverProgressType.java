package com.shop.tbms.enumerate.mold;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

@Getter
public enum MoldDeliverProgressType {
    SEND("Gửi"),
    RECEIVE("Nhận");

    MoldDeliverProgressType(String displayName) {
        this.displayName = displayName;
    }

    @JsonValue
    public int getValue() {return ordinal();}

    private String displayName;
}
