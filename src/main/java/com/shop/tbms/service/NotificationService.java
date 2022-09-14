package com.shop.tbms.service;

import com.shop.tbms.dto.noti.NotificationRequestDTO;
import com.shop.tbms.dto.noti.SubscriptionRequestDTO;

public interface NotificationService {
    void subscribeToTopic(SubscriptionRequestDTO requestDTO);

    void unsubscribeFromTopic(SubscriptionRequestDTO requestDTO);

    String sendPnsToTopic(NotificationRequestDTO requestDTO) throws Exception;
}
