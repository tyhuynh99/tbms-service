package com.shop.tbms.dto;

import com.shop.tbms.enumerate.mold.MoldElementType;
import lombok.*;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MoldElementDTO {
    private Long id;
    private String code;
    private String name;
    private String description;
    private MoldElementType type;
    private Boolean isRequired;
    private String possibleValue;
}
