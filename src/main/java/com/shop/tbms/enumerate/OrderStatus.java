package com.shop.tbms.enumerate;

import com.fasterxml.jackson.annotation.JsonValue;

public enum OrderStatus {
    IN_PROGRESS, COMPLETED;

    @JsonValue
    private Integer getValue() {return ordinal();}
}
