package com.shop.tbms.dto.step;

import lombok.*;

import javax.validation.constraints.NotNull;
import java.util.List;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ResetMoldStepReqDTO {
    @NotNull
    private Long resetToStepId;

    @NotNull
    private Long currentStepId;

    @NotNull
    private List<Long> listMoldId;
}
