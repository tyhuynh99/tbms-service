package com.shop.tbms.config.exception;

import com.shop.tbms.dto.ErrorRespDTO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.WebRequest;

import javax.validation.ValidationException;

@ControllerAdvice
@Slf4j
public class RestResponseExceptionHandler {
    @ExceptionHandler(BusinessException.class)
    public ResponseEntity<ErrorRespDTO> handleBusinessException(BusinessException e, WebRequest webRequest) {
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(buildErrorResponse(e));
    }

    @ExceptionHandler(ForbiddenException.class)
    public ResponseEntity<ErrorRespDTO> handleForbiddenException(ForbiddenException e, WebRequest webRequest) {
        return ResponseEntity.status(HttpStatus.FORBIDDEN).body(buildErrorResponse(e));
    }

    @ExceptionHandler(BindException.class)
    public ResponseEntity<ErrorRespDTO> handleMethodArgumentNotValidException(BindException e, WebRequest webRequest) {
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(buildErrorResponse(e));
    }

    @ExceptionHandler(ValidationException.class)
    public ResponseEntity<ErrorRespDTO> handleValidationException(ValidationException e, WebRequest webRequest) {
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(buildErrorResponse(e));
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<ErrorRespDTO> handleException(Exception e, WebRequest webRequest) {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(buildErrorResponse(e));
    }

    private ErrorRespDTO buildErrorResponse(BusinessException e) {
        return ErrorRespDTO.builder()
                .errorCode(e.getErrorCode())
                .errorMessage(e.getErrorMessage())
                .data(e.getData())
                .build();
    }

    private ErrorRespDTO buildErrorResponse(Exception e) {
        return ErrorRespDTO.builder()
                .errorCode(e.getMessage())
                .errorMessage(e.getMessage())
                .build();
    }

    private ErrorRespDTO buildErrorResponse(ForbiddenException e) {
        return ErrorRespDTO.builder()
                .errorMessage(e.getMessage())
                .build();
    }

    private ErrorRespDTO buildErrorResponse(BindException e) {
        return ErrorRespDTO.builder()
                .errorMessage(e.getMessage())
                .build();
    }

    private ErrorRespDTO buildErrorResponse(ValidationException e) {
        return ErrorRespDTO.builder()
                .errorMessage(e.getMessage())
                .build();
    }
}
