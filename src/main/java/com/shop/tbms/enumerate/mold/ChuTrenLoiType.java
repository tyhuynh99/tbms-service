package com.shop.tbms.enumerate.mold;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

@Getter
public enum ChuTrenLoiType {
    GIA_CONG_CNC_TRUC_TIEP("Gia công CNC trực tiếp"),
    BAN_DIEN("Bắn điện");

    private String description;

    ChuTrenLoiType(String description) {
        this.description = description;
    }

    @JsonValue
    public int getValue() {return ordinal();}
}
