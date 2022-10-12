package com.shop.tbms.service;

import com.shop.tbms.dto.NotificationDTO;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.noti.NotificationRequestDTO;
import com.shop.tbms.dto.noti.SubscriptionRequestDTO;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface NotificationService {
    void subscribeToTopic(SubscriptionRequestDTO requestDTO);

    void unsubscribeFromTopic(SubscriptionRequestDTO requestDTO);

    String sendPnsToTopic(NotificationRequestDTO requestDTO) throws Exception;

    Page<NotificationDTO> getListByUser(Pageable pageable);

    SuccessRespDTO readNotification(List<Long> listId);
}
