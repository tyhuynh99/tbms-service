package com.shop.tbms.dto.noti;

import lombok.*;

import java.util.List;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SubscriptionRequestDTO {
    String topicName;
    List<String> tokens;
}
