package com.shop.tbms.dto.order;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.shop.tbms.dto.mold.MoldDTO;
import com.shop.tbms.dto.mold.MoldGroupDetailReqDTO;
import com.shop.tbms.enumerate.order.OrderDisplayStatus;
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
    private List<MoldDTO> listMold;
    private List<MoldGroupDetailReqDTO> listMoldGroupDetail;
    private List<OrderStepRespDTO> listStep;
}
