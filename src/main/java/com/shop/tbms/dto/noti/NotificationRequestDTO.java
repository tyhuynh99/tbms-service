package com.shop.tbms.dto.noti;

import lombok.Data;

@Data
public class NotificationRequestDTO {
    private String target;
    private String title;
    private String body;
}
