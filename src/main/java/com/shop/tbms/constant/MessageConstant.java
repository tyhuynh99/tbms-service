package com.shop.tbms.constant;

import lombok.Getter;
import lombok.Setter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Component;

@Getter
@Setter
@Component
@PropertySource("classpath:message.properties")
public class MessageConstant {
    @Value("${create_success}")
    private String createSuccess;

    @Value("${update_success}")
    private String updateSuccess;
}
