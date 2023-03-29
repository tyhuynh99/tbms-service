package com.shop.tbms.enumerate.mold;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

@Getter
public enum VatLieuKhuonType {
    KHONG("Không"),
    NHOM("Nhôm"),
    SAT("Sắt");

    private String description;

    VatLieuKhuonType(String description) {
        this.description = description;
    }

    @JsonValue
    public int getValue() {return ordinal();}
}
