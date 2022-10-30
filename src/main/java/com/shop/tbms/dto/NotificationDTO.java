package com.shop.tbms.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.shop.tbms.enumerate.NotificationType;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class NotificationDTO {
    private Long id;
    private String receiverUsername;
    private String content;
    private NotificationType type;
    private boolean isRead;
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private LocalDateTime createdDate;
    private String createdDateDisplay;
}
