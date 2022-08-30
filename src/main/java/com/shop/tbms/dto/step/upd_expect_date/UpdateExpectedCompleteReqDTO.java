package com.shop.tbms.dto.step.upd_expect_date;

import lombok.*;

import javax.validation.constraints.NotNull;
import java.time.LocalDate;

@Getter
@Setter
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class UpdateExpectedCompleteReqDTO {
    @NotNull
    private Long stepId;

    @NotNull
    private LocalDate expectedCompleteDate;
}
