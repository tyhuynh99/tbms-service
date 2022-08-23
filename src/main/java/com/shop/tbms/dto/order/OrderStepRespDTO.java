package com.shop.tbms.dto.order;

import com.shop.tbms.enumerate.StepStatus;
import com.shop.tbms.enumerate.StepType;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;

@Getter
@Setter
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class OrderStepRespDTO implements Comparable<OrderStepRespDTO> {
    private Long id;
    private String code;
    private StepType type;
    private String name;
    private Integer sequenceNo;
    private Boolean resettable;
    private Boolean isStart;
    private Boolean isEnd;
    private LocalDate expectedCompleteDate;
    private StepStatus status;
    private BigDecimal percentComplete;

    @Override
    public int compareTo(OrderStepRespDTO o) {
        return this.sequenceNo.compareTo(o.getSequenceNo());
    }
}
