package com.shop.tbms.dto;

import lombok.*;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class EvidenceDTO {
    private Long id;
    private String description;
    private String url;
}
