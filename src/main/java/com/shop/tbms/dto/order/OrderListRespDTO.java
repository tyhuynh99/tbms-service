package com.shop.tbms.dto.order;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.shop.tbms.enumerate.order.OrderDisplayStatus;
import lombok.*;

import java.time.LocalDate;

@Getter
@Setter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class OrderListRespDTO {
    private Long id;
    private String code;
    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDate deliveredDate;
    private boolean isUrgent;
    private OrderDisplayStatus status;
    private Long numOfMold;
    private String procedureName;
}
