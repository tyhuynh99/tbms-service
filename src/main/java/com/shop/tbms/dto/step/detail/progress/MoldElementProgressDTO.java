package com.shop.tbms.dto.step.detail.progress;

import lombok.*;

import java.util.List;

@Getter @Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MoldElementProgressDTO {
    private String moldSize;
    private String moldSizeWithType;
    private long percentCompleted;
    private List<MoldElementProgressDetailDTO> listElement;
}
