package com.shop.tbms.dto.order;

import lombok.*;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.time.LocalDate;
import java.util.List;

@Getter
@Setter
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class OrderCreateReqDTO {
    @NotBlank
    private String procedureCode;

    @NotBlank
    private String code;

    @NotBlank
    private String customerName;

    private String soleFactoryName;

    @NotNull
    private LocalDate deliveredDate;

    @NotNull
    private Boolean isUrgent;

    private List<String> listSize;
}
