package com.shop.tbms.dto.order;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.shop.tbms.dto.MoldDTO;
import com.shop.tbms.dto.MoldElementDTO;
import com.shop.tbms.enumerate.OrderDisplayStatus;
import com.shop.tbms.enumerate.OrderStatus;
import lombok.*;

import java.time.LocalDate;
import java.util.List;

@Getter
@Setter
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class OrderDetailRespDTO {
    private Long id;
    private String code;
    private String customerName;
    private String soleFactoryName;
    private OrderDisplayStatus status;
    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDate deliveredDate;
    private String procedureName;
    private boolean isUrgent;
    private List<String> listMold;
    private List<MoldElementDTO> listMoldElement;
    private List<OrderStepRespDTO> listStep;
}
