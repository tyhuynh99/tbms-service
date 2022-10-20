package com.shop.tbms.constant;

import lombok.Getter;
import lombok.Setter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Component;

@Getter
@Setter
@Component
@PropertySource("classpath:noti-constant.properties")
public class NotificationConstant {
    @Value("${order.nearly_due.title}")
    private String orderNearlyDueTitle;

    @Value("${order.nearly_due.content}")
    private String orderNearlyDueContent;

    @Value("${order.overdue.title}")
    private String orderOverdueTitle;

    @Value("${order.overdue.content}")
    private String orderOverdueContent;

    @Value("${step.nearly_due.title}")
    private String stepNearlyDueTitle;

    @Value("${step.nearly_due.content}")
    private String stepNearlyDueContent;

    @Value("${step.overdue.title}")
    private String stepOverdueTitle;

    @Value("${step.overdue.content}")
    private String stepOverdueContent;
}
