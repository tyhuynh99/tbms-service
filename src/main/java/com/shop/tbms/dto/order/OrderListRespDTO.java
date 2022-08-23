package com.shop.tbms.dto.order;

import com.shop.tbms.enumerate.OrderStatus;
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
    private LocalDate deliveredDate;
    private Boolean isUrgent;
    private Boolean isLate;
    private OrderStatus status;
    private Long numOfMold;
    private String procedureName;
}
