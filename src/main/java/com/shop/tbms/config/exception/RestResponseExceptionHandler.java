package com.shop.tbms.config.exception;

import com.shop.tbms.dto.ErrorResponseDTO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.WebRequest;

@ControllerAdvice
@Slf4j
public class RestResponseExceptionHandler {
    @ExceptionHandler(BusinessException.class)
    public ResponseEntity<ErrorResponseDTO> handleBusinessException(BusinessException e, WebRequest webRequest) {
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(buildErrorResponse(e));
    }

    private ErrorResponseDTO buildErrorResponse(BusinessException e) {
        return ErrorResponseDTO.builder()
                .errorCode(e.getErrorCode())
                .errorMessage(e.getErrorMessage())
                .data(e.getData())
                .build();
    }
}
