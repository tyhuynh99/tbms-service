package com.shop.tbms.dto.step.detail.progress;

import lombok.*;

@Getter @Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MoldElementProgressDetailDTO {
    private Long progressId;
    private String elementName;
    private boolean isCompleted;
    private boolean canCheck;
    private boolean canUncheck;
}
