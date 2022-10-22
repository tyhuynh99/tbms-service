package com.shop.tbms.dto.log;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.*;

import java.time.LocalDate;

@Data
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ReportProgressCompleteDTO {
    private String mold;
    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDate completeAt;
}
