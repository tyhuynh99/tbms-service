package com.shop.tbms.enumerate.order;

import com.fasterxml.jackson.annotation.JsonValue;

public enum OrderPaymentStatus {
    PAID, NOT_PAID, OVERDUE_NOT_PAID;

    @JsonValue
    public int getValue() {return ordinal();}
}
