package com.shop.tbms.controller;

import com.shop.tbms.dto.ListWrapperDTO;
import com.shop.tbms.dto.NotificationDTO;
import com.shop.tbms.dto.PageResponse;
import com.shop.tbms.dto.SuccessRespDTO;
import com.shop.tbms.dto.noti.FBNotificationRequestDTO;
import com.shop.tbms.dto.noti.SubscriptionRequestDTO;
import com.shop.tbms.entity.TbmsNotification_;
import com.shop.tbms.service.NotificationService;
import com.shop.tbms.util.ResponseUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.SortDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/notification")
public class NotificationController {
    @Autowired
    private NotificationService notificationService;

    @PostMapping("/subscribe")
    public void subscribeToTopic(@RequestBody SubscriptionRequestDTO subscriptionRequestDto) {
        notificationService.subscribeToTopic(subscriptionRequestDto);
    }

    @PostMapping("/unsubscribe")
    public void unsubscribeFromTopic(SubscriptionRequestDTO subscriptionRequestDto) {
        notificationService.unsubscribeFromTopic(subscriptionRequestDto);
    }

    @PostMapping("/topic")
    public String sendPnsToTopic(@RequestBody FBNotificationRequestDTO FBNotificationRequestDto) throws Exception {
        return notificationService.sendPnsToTopic(FBNotificationRequestDto);
    }

    @GetMapping("/list")
    public ResponseEntity<PageResponse<NotificationDTO>> getListNotification(
            @SortDefault(
                    value = TbmsNotification_.CREATED_DATE,
                    direction = Sort.Direction.DESC)
                    Pageable pageable
    ) {
        return ResponseUtil.buildPageResponse(notificationService.getListByUser(pageable));
    }

    @PostMapping("/read")
    public ResponseEntity<SuccessRespDTO> readNotification(@RequestBody ListWrapperDTO<Long> listId) {
        return ResponseEntity.ok(notificationService.readNotification(listId.getData()));
    }

    @PostMapping("/test")
    public ResponseEntity<SuccessRespDTO> testNoti(@RequestBody FBNotificationRequestDTO notiReq) throws Exception {
        return ResponseEntity.ok(notificationService.testNoti(notiReq));
    }
}
