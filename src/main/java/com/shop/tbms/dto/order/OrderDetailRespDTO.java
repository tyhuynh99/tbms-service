package com.shop.tbms.dto.order;

import com.shop.tbms.dto.MoldDTO;
import com.shop.tbms.dto.MoldElementDTO;
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
    private OrderStatus status;
    private LocalDate deliveredDate;
    private String procedureName;
    private List<MoldDTO> listMold;
    private List<MoldElementDTO> listMoldElement;
    private List<OrderStepRespDTO> listStep;
}
