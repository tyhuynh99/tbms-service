package com.shop.tbms.config.exception;

import lombok.*;

import java.util.List;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BusinessException extends RuntimeException {
    private String errorMessage;
    private String errorCode;
    private List<?> data;

    public BusinessException(String message) {
        super(message);
        this.errorMessage = message;
    }

    public BusinessException(String message, List<?> data) {
        super(message);
        this.errorMessage = message;
        this.data = data;
    }
}
