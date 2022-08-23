package com.shop.tbms.util;

import com.shop.tbms.dto.PageResponse;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;


public interface ResponseUtil {
    static <X> ResponseEntity<PageResponse<X>> buildPageResponse(Page<X> page) {
        return buildPageResponse(page, (HttpHeaders)null);
    }
    static <X> ResponseEntity<PageResponse<X>> buildPageResponse(Page<X> page, HttpHeaders headers) {
        PageResponse<X> body = new PageResponse(page.getContent(), page.getTotalPages(), page.getTotalElements());
        return ((ResponseEntity.BodyBuilder)ResponseEntity.ok().headers(headers)).body(body);
    }
}
