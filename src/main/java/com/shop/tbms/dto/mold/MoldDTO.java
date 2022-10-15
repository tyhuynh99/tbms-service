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
    private String sizeWithType;
}
