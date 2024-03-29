package com.shop.tbms.dto.step.detail.progress;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.shop.tbms.enumerate.mold.MoldDeliverProgressType;
import lombok.*;

import java.time.LocalDateTime;

@Getter @Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MoldDeliverProgressDTO {
    private Long progressId;
    private boolean isCompleted;
    private MoldDeliverProgressType type;

    @JsonFormat(pattern = "HH:mm yyyy-MM-dd")
    private LocalDateTime actionDate;

    private String moldSize;
    private Long moldId;
    private String moldSizeWithType;
    private boolean canCheck;
    private boolean canUncheck;
}
