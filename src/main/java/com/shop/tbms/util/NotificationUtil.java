package com.shop.tbms.util;

import com.shop.tbms.constant.NotificationConstant;
import com.shop.tbms.dto.noti.FBNotificationRequestDTO;
import com.shop.tbms.entity.PurchaseOrder;
import com.shop.tbms.entity.Step;
import com.shop.tbms.entity.TbmsNotification;
import com.shop.tbms.enumerate.NotificationType;

import java.util.Optional;

public class NotificationUtil {

    public static FBNotificationRequestDTO genNotiOrderNearlyDue(PurchaseOrder order, NotificationConstant notificationConstant, String receiverUsername) {
        return FBNotificationRequestDTO.builder()
                .title(notificationConstant.getOrderNearlyDueTitle())
                .body(String.format(notificationConstant.getOrderNearlyDueContent(), order.getCode()))
                .topic(receiverUsername)
                .build();
    }

    public static FBNotificationRequestDTO genNotiOrderOverdue(PurchaseOrder order, NotificationConstant notificationConstant, String receiverUsername) {
        return FBNotificationRequestDTO.builder()
                .title(notificationConstant.getOrderOverdueTitle())
                .body(String.format(notificationConstant.getOrderOverdueContent(), order.getCode()))
                .topic(receiverUsername)
                .orderId(order.getId())
                .build();
    }

    public static FBNotificationRequestDTO genNotiStepNearlyLate(Step step, NotificationConstant notificationConstant, String receiverUsername) {
        return FBNotificationRequestDTO.builder()
                .title(notificationConstant.getStepNearlyDueTitle())
                .body(String.format(notificationConstant.getStepNearlyDueContent(), step.getName(), step.getProcedure().getPurchaseOrder().getCode()))
                .topic(receiverUsername)
                .orderId(step.getProcedure().getOrderId())
                .build();
    }

    public static FBNotificationRequestDTO genNotiStepLate(Step step, NotificationConstant notificationConstant, String receiverUsername) {
        return FBNotificationRequestDTO.builder()
                .title(notificationConstant.getStepOverdueTitle())
                .body(String.format(notificationConstant.getStepOverdueContent(), step.getName(), step.getProcedure().getPurchaseOrder().getCode()))
                .topic(receiverUsername)
                .orderId(step.getProcedure().getOrderId())
                .build();
    }

    public static TbmsNotification genEntityNotification(FBNotificationRequestDTO fbNotificationRequestDTO, Step step, NotificationType type) {
        TbmsNotification notification = new TbmsNotification();

        notification.setIsRead(Boolean.FALSE);
        notification.setReceiverUsername(fbNotificationRequestDTO.getTopic());
        notification.setContent(fbNotificationRequestDTO.getBody());
        notification.setType(type);
        notification.setOrderId(Optional
                .ofNullable(step)
                .map(step1 -> step.getProcedure().getOrderId())
                .orElse(null)
        );

        return notification;
    }

    public static TbmsNotification genEntityNotification(FBNotificationRequestDTO fbNotificationRequestDTO, NotificationType type) {
        TbmsNotification notification = new TbmsNotification();

        notification.setIsRead(Boolean.FALSE);
        notification.setReceiverUsername(fbNotificationRequestDTO.getTopic());
        notification.setContent(fbNotificationRequestDTO.getBody());
        notification.setType(type);
        notification.setOrderId(fbNotificationRequestDTO.getOrderId());

        return notification;
    }
}
