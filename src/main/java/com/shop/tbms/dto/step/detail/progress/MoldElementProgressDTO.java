package com.shop.tbms.dto.step.detail.progress;

import lombok.*;

import java.util.List;

@Data
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MoldElementProgressDTO {
    private String moldSize;
    private long percentCompleted;
    private List<MoldElementProgressDetailDTO> listElement;
}
