package com.shop.tbms.enumerate;

import com.fasterxml.jackson.annotation.JsonValue;

public enum OrderPaymentStatus {
    PAID, NOT_PAID, OVERDUE_NOT_PAID;

    @JsonValue
    public int getValue() {return ordinal();}
}
