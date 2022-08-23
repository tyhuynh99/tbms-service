package com.shop.tbms.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Getter;
import lombok.Setter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Getter
@Setter
public class PageResponse<X> {
    private List<X> data;
    private int totalPages;
    private long totalElements;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Map<String, Object> attributes;

    public PageResponse() {
    }
    public PageResponse(List<X> content, int totalPages, long totalElements) {
        this.data = content;
        this.totalPages = totalPages;
        this.totalElements = totalElements;
    }
    public void addAttribute(String key, Object value) {
        if (attributes == null) {
            attributes = new HashMap<>();
        }
        attributes.put(key, value);
    }
}
