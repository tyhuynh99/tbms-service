package com.shop.tbms.dto.order;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.shop.tbms.enumerate.StepStatus;
import com.shop.tbms.enumerate.StepType;
import lombok.*;

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
    private boolean resettable;
    private boolean isStart;
    private boolean isEnd;
    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDate expectedCompleteDate;
    private StepStatus status;
    private Long percentComplete;

    @Override
    public int compareTo(OrderStepRespDTO o) {
        return this.sequenceNo.compareTo(o.getSequenceNo());
    }
}
