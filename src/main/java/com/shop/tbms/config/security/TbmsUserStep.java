package com.shop.tbms.config.security;

import lombok.*;

@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TbmsUserStep {
    private String stepCode;
    private String stepName;
}
