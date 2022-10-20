package com.shop.tbms.service.impl;

import com.google.auth.oauth2.GoogleCredentials;
import com.google.firebase.FirebaseApp;
import com.google.firebase.FirebaseOptions;
import com.google.firebase.messaging.FirebaseMessaging;
import com.google.firebase.messaging.FirebaseMessagingException;
import com.google.firebase.messaging.Message;
import com.google.firebase.messaging.Notification;
import com.shop.tbms.constant.MessageConstant;
import com.shop.tbms.dto.NotificationDTO;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.noti.FBNotificationRequestDTO;
import com.shop.tbms.dto.noti.SubscriptionRequestDTO;
import com.shop.tbms.entity.TbmsNotification;
import com.shop.tbms.enumerate.NotificationType;
import com.shop.tbms.mapper.NotificationMapper;
import com.shop.tbms.repository.NotificationRepository;
import com.shop.tbms.service.NotificationService;
import com.shop.tbms.util.AuthenticationUtil;
import com.shop.tbms.util.NotificationUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.PostConstruct;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.List;

@Service
@Transactional
@Slf4j
public class NotificationServiceImpl implements NotificationService {
    @Value("${firebase.admin-key}")
    private String firebaseKeyFile;

    private FirebaseApp firebaseApp;

    @Autowired
    private NotificationRepository notificationRepository;

    @Autowired
    private NotificationMapper notificationMapper;

    @PostConstruct
    private void initialize() throws Exception {
        try {
            FileInputStream serviceAccount = new FileInputStream(firebaseKeyFile);
            FirebaseOptions options = FirebaseOptions.builder()
                    .setCredentials(GoogleCredentials.fromStream(serviceAccount))
                    .build();

            if (FirebaseApp.getApps().isEmpty()) {
                this.firebaseApp = FirebaseApp.initializeApp(options);
            } else {
                this.firebaseApp = FirebaseApp.getInstance();
            }
        } catch (IOException e) {
            log.error("Initialize FCM error", e);
            throw new Exception(e);
        }
        log.info("Initialize FCM successful !");
    }

    @Override
    public void subscribeToTopic(SubscriptionRequestDTO requestDTO) {
        log.info("Start subcribe topic {}", requestDTO);
        try {
            FirebaseMessaging.getInstance(firebaseApp).subscribeToTopic(requestDTO.getTokens(), requestDTO.getTopicName());
        } catch (FirebaseMessagingException e) {
            log.error("Firebase subscribe to topic failed !", e);
            log.error(String.format("Topic: %s", requestDTO.getTopicName()));
            log.error(String.format("Tokens: %s", requestDTO.getTokens().toString()));
        }
    }

    @Override
    public void unsubscribeFromTopic(SubscriptionRequestDTO requestDTO) {
        log.info("Start unsubcribe topic {}", requestDTO);
        try {
            FirebaseMessaging.getInstance(firebaseApp).unsubscribeFromTopic(requestDTO.getTokens(), requestDTO.getTopicName());
        } catch (FirebaseMessagingException e) {
            log.error("Firebase unsubscribe to topic failed !", e);
            log.error(String.format("Topic: %s", requestDTO.getTopicName()));
            log.error(String.format("Tokens: %s", requestDTO.getTokens().toString()));
        }
    }

    @Override
    public String sendPnsToTopic(FBNotificationRequestDTO requestDTO) throws Exception {
        log.info("Start send noti {}", requestDTO);
        String title = requestDTO.getTitle();
        String body = requestDTO.getBody();
        String topic = requestDTO.getTopic();

        Notification notification = Notification.builder()
                .setBody(body)
                .setTitle(title)
                .build();

        Message message = Message.builder()
                .setTopic(topic)
                .setNotification(notification)
                .putData("content", title)
                .putData("body", body)
                .build();

        try {
            return FirebaseMessaging.getInstance().send(message);
        } catch (FirebaseMessagingException e) {
            log.error("Fail to send firebase notification", e);
            throw new Exception(e);
        }


    }

    @Override
    public Page<NotificationDTO> getListByUser(Pageable pageable) {
        return notificationRepository.findByReceiverUsername(
                AuthenticationUtil.getUserDetails().getUsername(),
                pageable
        ).map(notificationMapper::toDTO);
    }

    @Override
    public SuccessRespDTO readNotification(List<Long> listId) {
        List<TbmsNotification> tbmsNotificationList = notificationRepository
                .findByIdInAndReceiverUsername(
                        listId,
                        AuthenticationUtil.getUserDetails().getUsername());

        tbmsNotificationList.forEach(tbmsNotification -> tbmsNotification.setIsRead(Boolean.TRUE));

        notificationRepository.saveAll(tbmsNotificationList);
        return SuccessRespDTO.builder()
                .message(MessageConstant.UPDATE_SUCCESS)
                .build();
    }

    @Override
    public SuccessRespDTO testNoti(FBNotificationRequestDTO requestDTO) throws Exception {
        sendPnsToTopic(requestDTO);
        notificationRepository.save(NotificationUtil.genEntityNotification(requestDTO, null, NotificationType.OTHER));

        return null;
    }
}
