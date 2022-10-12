package com.shop.tbms.enumerate.mold;

import com.fasterxml.jackson.annotation.JsonValue;

public enum MoldStructure {
    GO_DUC,
    GO_DUC_N_KHOANG_GO,
    KHOANG_GO_N_IN;

    @JsonValue
    public int getValue() {return ordinal();}
}
