package com.shop.tbms.dto.mold;

import lombok.*;

import java.util.List;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MoldGroupReqDTO {
    private long orderId;
    private MoldGroupDetailDTO moldGroup;
}
