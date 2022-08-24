package com.shop.tbms.dto.order;

import lombok.*;

@Getter
@Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class OrderFilterReqDTO {
    private String codeContains;
    private String procedureCodeEqual;
    private Integer orderStatusEqual;
    private Integer orderPaymentStatusEqual;
}
