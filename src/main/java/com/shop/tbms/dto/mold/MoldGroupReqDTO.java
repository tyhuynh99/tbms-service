package com.shop.tbms.dto.mold;

import lombok.*;

@Getter @Setter
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MoldGroupReqDTO {
    private long orderId;
    private MoldGroupDetailReqDTO moldGroup;
}
