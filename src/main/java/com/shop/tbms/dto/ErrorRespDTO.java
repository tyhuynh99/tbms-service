package com.shop.tbms.dto;

import lombok.*;

import java.util.List;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ErrorRespDTO {
    private String errorMessage;
    private String errorCode;
    private List<?> data;
}
