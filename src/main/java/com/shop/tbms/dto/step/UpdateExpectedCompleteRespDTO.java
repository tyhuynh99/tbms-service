package com.shop.tbms.dto.step;

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
