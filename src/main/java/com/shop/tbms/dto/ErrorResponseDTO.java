package com.shop.tbms.dto;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ErrorResponseDTO {
    private String errorMessage;
    private String errorCode;
}
