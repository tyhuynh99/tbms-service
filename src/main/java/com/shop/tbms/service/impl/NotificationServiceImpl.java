package com.shop.tbms.service.impl;

import com.google.auth.oauth2.GoogleCredentials;
import com.google.firebase.FirebaseApp;
import com.google.firebase.FirebaseOptions;
import com.google.firebase.messaging.FirebaseMessaging;
import com.google.firebase.messaging.FirebaseMessagingException;
import com.google.firebase.messaging.Message;
import com.google.firebase.messaging.Notification;
import com.shop.tbms.dto.noti.NotificationRequestDTO;
import com.shop.tbms.dto.noti.SubscriptionRequestDTO;
import com.shop.tbms.service.NotificationService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.io.FileInputStream;
import java.io.IOException;

@Service
@Slf4j
public class NotificationServiceImpl implements NotificationService {
    @Value("${firebase.admin-key}")
    private String firebaseKeyFile;

    private FirebaseApp firebaseApp;

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
        try {
            FirebaseMessaging.getInstance(firebaseApp).unsubscribeFromTopic(requestDTO.getTokens(), requestDTO.getTopicName());
        } catch (FirebaseMessagingException e) {
            log.error("Firebase unsubscribe to topic failed !", e);
            log.error(String.format("Topic: %s", requestDTO.getTopicName()));
            log.error(String.format("Tokens: %s", requestDTO.getTokens().toString()));
        }
    }

    @Override
    public String sendPnsToTopic(NotificationRequestDTO requestDTO) throws Exception {
        String title = requestDTO.getTitle();
        String body = requestDTO.getBody();
        String target = requestDTO.getTarget();

        Notification notification = Notification.builder()
                .setBody(body)
                .setTitle(title)
                .build();

        Message message = Message.builder()
                .setTopic(target)
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
}
