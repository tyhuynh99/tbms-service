package com.shop.tbms.dto.mold;

import lombok.*;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MoldDTO {
    private Long id;
    private String size;

    @Data
    @ToString
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MoldGroupElementDTO {
        private String name;
        private boolean checked;
    }
}
