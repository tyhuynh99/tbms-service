package com.shop.tbms.dto.mold;

import lombok.*;

import javax.validation.constraints.NotBlank;

@Getter @Setter
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class MoldElementDTO {
    @NotBlank
    private String name;
    private boolean checked;
}
