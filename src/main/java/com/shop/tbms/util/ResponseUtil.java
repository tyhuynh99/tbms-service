package com.shop.tbms.util;

import com.shop.tbms.dto.ListWrapperDTO;
import com.shop.tbms.dto.PageResponse;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;

import java.util.List;


public interface ResponseUtil {
    static <X> ResponseEntity<PageResponse<X>> buildPageResponse(Page<X> page) {
        return buildPageResponse(page, (HttpHeaders)null);
    }
    static <X> ResponseEntity<PageResponse<X>> buildPageResponse(Page<X> page, HttpHeaders headers) {
        PageResponse<X> body = new PageResponse(page.getContent(), page.getTotalPages(), page.getTotalElements());
        return ((ResponseEntity.BodyBuilder)ResponseEntity.ok().headers(headers)).body(body);
    }
    static <X> ListWrapperDTO<X> wrapListToResp(List<X> listData) {
        return new ListWrapperDTO<X>(listData);
    }
}
