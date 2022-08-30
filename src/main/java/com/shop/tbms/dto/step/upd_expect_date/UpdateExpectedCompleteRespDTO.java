package com.shop.tbms.dto.step.upd_expect_date;

import lombok.*;

import javax.validation.constraints.NotNull;

@Getter
@Setter
@Builder
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class UpdateExpectedCompleteRespDTO {
    private Long stepId;
}
