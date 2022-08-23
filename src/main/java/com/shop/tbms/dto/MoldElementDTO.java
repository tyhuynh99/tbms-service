package com.shop.tbms.dto;

import com.shop.tbms.enumerate.MoldElementType;
import lombok.*;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

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
