package com.shop.tbms.dto.mold;

import lombok.*;

import javax.validation.constraints.NotBlank;

@Data
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class MoldElementDTO {
    private Long id;
    @NotBlank
    private String name;
    private boolean checked;
}
