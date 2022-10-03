package com.shop.tbms.dto.step.detail.progress;

import com.shop.tbms.enumerate.mold.MoldDeliverProgressType;
import lombok.*;

import java.time.LocalDateTime;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MoldDeliverProgressDTO {
    private Long progressId;
    private boolean isCompleted;
    private MoldDeliverProgressType type;
    private LocalDateTime actionAt;
    private String moldSize;
}
