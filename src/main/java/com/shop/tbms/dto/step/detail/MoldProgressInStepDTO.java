package com.shop.tbms.dto.step.detail;

import lombok.*;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MoldProgressInStepDTO {
    private Long progressId;
    private Boolean isCompleted;
    private Long moldId;
    private String moldSize;
}
