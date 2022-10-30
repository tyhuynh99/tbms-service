package com.shop.tbms.dto.noti;

import lombok.*;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FBNotificationRequestDTO {
    private String topic;
    private String title;
    private String body;
    private Long orderId;
}
