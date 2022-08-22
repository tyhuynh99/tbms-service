package com.shop.tbms.enumerate;

import com.fasterxml.jackson.annotation.JsonValue;

import java.util.stream.Stream;

public enum Role {
    PRESIDENT, ACCOUNTANT, SECRETARY, EMPLOYEE;


    @JsonValue
    public int getValue() {return ordinal();}

    public static Role fromValue(int value) {
        return Stream.of(Role.values())
                .filter(role -> role.ordinal() == value)
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Invalid value of Role"));
    }
}
