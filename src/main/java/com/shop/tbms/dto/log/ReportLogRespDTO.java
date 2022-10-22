package com.shop.tbms.dto.log;

import lombok.*;

import java.util.List;

@Data
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ReportLogRespDTO {
    private List<ReportProgressCompleteDTO> completeTime = List.of();
    private List<ReportLogDetailDTO> log = List.of();
}
