package com.shop.tbms.enumerate;

import com.fasterxml.jackson.annotation.JsonValue;

import java.util.stream.Stream;

public enum CheckedValue {
    UNCHECKED,
    CHECKED;

    public static CheckedValue fromValue(String value) {
        int intVal = Integer.parseInt(value);

        return Stream.of(CheckedValue.values())
                .filter(checkedValue -> checkedValue.ordinal() == intVal)
                .findFirst()
                .orElseThrow(IllegalArgumentException::new);
    }
}
