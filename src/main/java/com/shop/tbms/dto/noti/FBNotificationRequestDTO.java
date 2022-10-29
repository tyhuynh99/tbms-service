package com.shop.tbms.dto.noti;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FBNotificationRequestDTO {
    private String topic;
    private String title;
    private String body;
    private Long orderId;
}
