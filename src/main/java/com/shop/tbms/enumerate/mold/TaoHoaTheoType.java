package com.shop.tbms.enumerate.mold;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

@Getter
public enum TaoHoaTheoType {
    THEO_MAU_DE("Theo mẫu đế"),
    THEO_BAN_VE("Theo bản vẽ");

    private String description;

    TaoHoaTheoType(String description) {
        this.description = description;
    }

    @JsonValue
    public int getValue() {
        return ordinal();
    }
}
