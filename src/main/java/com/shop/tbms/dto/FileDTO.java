package com.shop.tbms.dto;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class FileDTO {
    private String url;
    private String filename;
}
