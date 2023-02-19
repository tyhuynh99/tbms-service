package com.shop.tbms.enumerate.mold;

import com.fasterxml.jackson.annotation.JsonValue;

public enum MoldType {
    RB,
    CM_EVA,
    TPR,
    IP,
    PU,
    FOOTBED,
    FREEFORM;

    @JsonValue
    public int getValue() {return ordinal();}
}
