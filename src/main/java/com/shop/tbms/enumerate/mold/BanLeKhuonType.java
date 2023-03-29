package com.shop.tbms.enumerate.mold;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

@Getter
public enum BanLeKhuonType {
    KHONG("Không"),
    KHUON_BAN_LE_2_BEN("Khuôn hàn bản lề 2 bên"),
    KHUON_HAN_ONG("Khuôn hàn ống"),
    KHUON_HANG_BAN_LE_ONG("Khuôn hàn bản lề + ống"),
    KHUON_HAN_TAY("Khuôn hàn tay");

    private String description;

    BanLeKhuonType(String description) {
        this.description = description;
    }

    @JsonValue
    public int getValue() {
        return ordinal();
    }
}
