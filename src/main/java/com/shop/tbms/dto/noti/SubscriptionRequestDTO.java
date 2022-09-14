package com.shop.tbms.dto.noti;

import lombok.Data;

import java.util.List;

@Data
public class SubscriptionRequestDTO {
    String topicName;
    List<String> tokens;
}
