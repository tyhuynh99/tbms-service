package com.shop.tbms.dto.step.detail.progress;

import lombok.*;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MoldProgressDTO {
    private Long progressId;
    private Boolean isCompleted;
    private Long moldId;
    private String moldSize;
}
